{-|
Module      : Modsefa.Examples.Subscription.Test
Description : Example test functions for the Modsefa Subscription application.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module provides entry points ('IO' actions) for running predefined test
scenarios against the Subscription example application ('SubscriptionApp'). These
scenarios utilize the client functions defined in
"Modsefa.Examples.Subscription.Client" to execute actions like service
initialization, tier creation, subscribing, and withdrawing funds.

It includes both positive test scenarios verifying successful execution and
negative test scenarios that use the corruption framework
('Modsefa.Test.RefAwareMutation') to ensure validator robustness against
invalid inputs or state transitions.

Scenarios typically require network configuration ("config.json"),
signing keys ("spender.skey"), and a bootstrap 'TxOutRef' hash/index
(provided as arguments) to identify the application instance on a testnet.
It also includes a developer utility ('runSubscriptionIR') for inspecting the
generated Intermediate Representation.
-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Modsefa.Examples.Subscription.Test
  ( -- * Test Scenario Runners
    runSubscriptionInitTest
  , runCreatePricingTierTest
  , runUpdateServiceConfigTest
  , runUpdateServiceProviderTest
  , runBatchCreateCouponsTest
  , runBatchDeleteCouponsTest
  , runSubscribeTest
  , runSubscribeWithCouponTest
  , runWithdrawTreasuryTest
    -- * Negative Test Scenarios (Corruption)
  , runSubscriptionCorruptedMintingTest
  , runSubscriptionCorruptedSubscribeTest
  , runSubscriptionCorruptedInstanceRefTest
  , runSubscriptionCorruptedPaymentTest
  , runSubscriptionCorruptProviderUpdateTest
    -- * Developer Utilities
  , runSubscriptionIR
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Data.Maybe (isJust, listToMaybe, maybeToList)
import Data.Proxy (Proxy(Proxy))
import Data.Text (pack, unpack)
import Text.Pretty.Simple (pPrint)

import GeniusYield.TxBuilder
  ( GYTxQueryMonad(utxosAtAddress), buildTxBody, runGYTxBuilderMonadIO
  , runGYTxQueryMonadIO
  )
import GeniusYield.Types
  ( AsPubKeyHash(toPubKeyHash), GYAssetClass(GYLovelace), GYProviders(gySubmitTx)
  , GYUTxO(utxoRef), addressFromPaymentKeyHash, addressToPlutus, assetClassToPlutus
  , generateSigningKey, getVerificationKey, pubKeyHashToPlutus, readSigningKey
  , signGYTxBody, utxosToList, valueFromLovelace, verificationKeyHash
  )
import GeniusYield.Types.KeyRole (GYKeyRole(..))
import PlutusLedgerApi.V3 (BuiltinByteString, POSIXTime, PubKeyHash, toData)

import Modsefa.Client
  ( ClientEnv(..), ModsefaClient(..), StateInstance(..), Wallet(wAddress)
  , getPkhFromSKeyFile, gyTxOutRefToV3, loadModsefaClient, queryAndPrintState
  , queryStateInstances, runAction, withClientEnv, (+:), pattern End
  )
import Modsefa.Core.Foundation (SStateType(SStateType))
import Modsefa.Core.IR.Compiler (compileIR)
import Modsefa.Core.Singletons(SStateRef(STypedTheOnlyInstance), autoSingletonFull)
import Modsefa.Test.Harness (handleResult, runScenario)
import Modsefa.Test.RefAwareMutation
  ( ActionMutation(..), ActionMutationStrategy(..), buildRefCorruptedTransaction
  )

import Modsefa.Examples.Subscription.Client
  ( appInstanceBootstrapRef, mkSubscriptionAppInstance
  )
import Modsefa.Examples.Subscription.Scripts ()
import Modsefa.Examples.Subscription.Spec
  ( BatchCreateCouponsSpec, BatchDeleteCouponsSpec, CreatePricingTierSpec
  , InitializeServiceSpec, SubscribeSpec, SubscribeWithCouponSpec, SubscriptionApp
  , UpdateServiceConfigSpec, UpdateServiceProviderSpec, WithdrawTreasurySpec
  )
import Modsefa.Examples.Subscription.Types
  ( Coupon(..), CouponState, CustomerSubscription(..), CustomerSubscriptionState
  , PricingTierState, ServiceConfig(serviceConfigName), ServiceConfigState
  , TreasuryAdaState
  )


-- ============================================================================
-- * Test Scenario Runners
-- ============================================================================

-- | Test scenario runner for initializing the SubscriptionApp using a specific bootstrap UTxO.
runSubscriptionInitTest :: String -> Integer -> IO ()
runSubscriptionInitTest txHashStr txIndex = do
  let eAppInst = mkSubscriptionAppInstance txHashStr txIndex
  case eAppInst of
    Left err -> putStrLn $ "❌ ERROR: Could not create App Instance: " ++ unpack err
    Right appInstance -> do
      
      -- Define the action to run
      let actionToRun client = do
            let (providerPkh, _) = head (mcSigners client)
            let serviceName    = "Test Subscription Service" :: BuiltinByteString
            let tierName       = "Basic Tier" :: BuiltinByteString
            let price          = 10000000 :: Integer
            let assetClass     = assetClassToPlutus GYLovelace
            let billingPeriod  = 2592000000 :: POSIXTime
            let contractLength = 31536000000 :: POSIXTime

            -- Use a flat tuple
            let paramsTuple = (serviceName, providerPkh, tierName, price, assetClass, billingPeriod, contractLength)

            runAction client appInstance (Proxy @InitializeServiceSpec) paramsTuple

      let verify clientEnv appInst = do
            mConfigInstance <- listToMaybe <$> queryStateInstances (Proxy @ServiceConfigState) clientEnv appInstance
            tierInstances <- queryStateInstances (Proxy @PricingTierState) clientEnv appInstance

            putStrLn "Querying ServiceConfigState..."
            queryAndPrintState (const $ pure mConfigInstance) maybeToList appInst
            putStrLn "\nQuerying PricingTierState..."
            queryAndPrintState (const $ pure tierInstances) id appInst

            let configFound = isJust mConfigInstance
            let correctTierCount = length tierInstances == 1
            return (configFound && correctTierCount)

      runScenario
        "InitializeService"
        "config.json"
        "keys/spender.skey"
        appInstance
        actionToRun
        verify

-- | Test scenario runner for creating a new pricing tier.
runCreatePricingTierTest :: String -> Integer -> IO ()
runCreatePricingTierTest txHashStr txIndex = do
  let eAppInst = mkSubscriptionAppInstance txHashStr txIndex
  case eAppInst of
    Left err -> putStrLn $ "❌ ERROR: Could not create App Instance: " ++ unpack err
    Right appInstance -> do

      let actionToRun client = do
            let tierName       = "Premium Tier" :: BuiltinByteString
            let price          = 50000000 :: Integer
            let assetClass     = assetClassToPlutus GYLovelace
            let billingPeriod  = 2592000000 :: POSIXTime
            let contractLength = 31536000000 :: POSIXTime

            let paramsTuple = (tierName, price, assetClass, billingPeriod, contractLength)

            runAction client appInstance (Proxy @CreatePricingTierSpec) paramsTuple

      let verify clientEnv appInst = do
            tierInstances <- queryStateInstances (Proxy @PricingTierState) clientEnv appInstance
            putStrLn "Querying PricingTierState..."
            queryAndPrintState (const $ pure tierInstances) id appInst
            return (length tierInstances > 1) -- Assumes init tier exists

      runScenario
        "CreatePricingTier"
        "config.json"
        "keys/spender.skey"
        appInstance
        actionToRun
        verify

-- | Test scenario runner for updating the service configuration name.
runUpdateServiceConfigTest :: String -> Integer -> IO ()
runUpdateServiceConfigTest txHashStr txIndex = do
  let eAppInst = mkSubscriptionAppInstance txHashStr txIndex
  case eAppInst of
    Left err -> putStrLn $ "❌ ERROR: Could not create App Instance: " ++ unpack err
    Right appInstance -> do

      let actionToRun client = do
            let paramsTuple :: BuiltinByteString
                paramsTuple = "Updated Subscription Service Name"
            runAction client appInstance (Proxy @UpdateServiceConfigSpec) paramsTuple

      let verify clientEnv appInst = do
            mConfigInstance <- listToMaybe <$> queryStateInstances (Proxy @ServiceConfigState) clientEnv appInstance
            putStrLn "Querying ServiceConfigState..."
            queryAndPrintState (const $ pure mConfigInstance) maybeToList appInst
            let newName = "Updated Subscription Service Name"
            let nameMatches = case mConfigInstance of
                                Just configInst -> serviceConfigName (siData configInst) == newName
                                Nothing         -> False
            return nameMatches

      runScenario
        "UpdateServiceConfig"
        "config.json"
        "keys/spender.skey"
        appInstance
        actionToRun
        verify

-- | Test scenario runner for updating the service provider PKH.
runUpdateServiceProviderTest :: String -> Integer -> IO ()
runUpdateServiceProviderTest txHashStr txIndex = do
  let eAppInst = mkSubscriptionAppInstance txHashStr txIndex
  case eAppInst of
    Left err -> putStrLn $ "❌ ERROR: Could not create App Instance: " ++ unpack err
    Right appInstance -> do

      let actionToRun client = do
            newSKey <- generateSigningKey @'GYKeyRolePayment
            let newProviderPkh = pubKeyHashToPlutus $ toPubKeyHash $ verificationKeyHash $ getVerificationKey newSKey
            putStrLn $ "✓ Generated new provider PKH: " ++ show newProviderPkh

            let paramsTuple = newProviderPkh
            runAction client appInstance (Proxy @UpdateServiceProviderSpec) paramsTuple

      let verify clientEnv appInst = do
            mConfigInstance <- listToMaybe <$> queryStateInstances (Proxy @ServiceConfigState) clientEnv appInstance
            putStrLn "Querying ServiceConfigState..."
            queryAndPrintState (const $ pure mConfigInstance) maybeToList appInst
            return (isJust mConfigInstance) -- Simple success check

      runScenario
        "UpdateServiceProvider"
        "config.json"
        "keys/spender.skey"
        appInstance
        actionToRun
        verify

-- | Test scenario runner for creating a batch of coupons.
runBatchCreateCouponsTest :: String -> Integer -> IO ()
runBatchCreateCouponsTest txHashStr txIndex = do
  let eAppInst = mkSubscriptionAppInstance txHashStr txIndex
  case eAppInst of
    Left err -> putStrLn $ "❌ ERROR: Could not create App Instance: " ++ unpack err
    Right appInstance -> do

      let actionToRun client = do
            let providers = ceProviders (mcEnv client)
            let networkId = ceNetworkId (mcEnv client)
            let ownAddress = wAddress (mcWallet client)
            
            putStrLn $ "Querying wallet for UTxO to use as batchId at address: " ++ show ownAddress
            eUtxos <- try $ runGYTxQueryMonadIO networkId providers $ utxosAtAddress ownAddress Nothing
            case eUtxos of
              Left (e :: SomeException) -> return $ Left $ "Failed to query wallet UTxOs: " <> pack (show e)
              Right walletUtxosMap -> case listToMaybe (utxosToList walletUtxosMap) of
                Nothing -> return $ Left "No spendable UTxOs found in wallet to use as batchId."
                Just utxoToSpend -> do
                  let batchIdRefV3 = gyTxOutRefToV3 (utxoRef utxoToSpend)
                  putStrLn $ "✓ Selected UTxO for batchId: " ++ show batchIdRefV3

                  let coupons =
                        [ Coupon { couponId = 101, couponDiscountPercent = 10, couponBatchId = batchIdRefV3 }
                        , Coupon { couponId = 102, couponDiscountPercent = 20, couponBatchId = batchIdRefV3 }
                        ]

                  let paramsTuple = (coupons, batchIdRefV3)
                  runAction client appInstance (Proxy @BatchCreateCouponsSpec) paramsTuple

      let verify clientEnv appInst = do
            couponInstances <- queryStateInstances (Proxy @CouponState) clientEnv appInstance
            putStrLn "Querying CouponState..."
            queryAndPrintState (const $ pure couponInstances) id appInst
            return (length couponInstances >= 2)

      runScenario
        "BatchCreateCoupons"
        "config.json"
        "keys/spender.skey"
        appInstance
        actionToRun
        verify

-- | Test scenario runner for deleting a batch of coupons.
--   Queries existing coupons to find a batch to delete and verifies the count.
runBatchDeleteCouponsTest :: String -> Integer -> IO ()
runBatchDeleteCouponsTest txHashStr txIndex = do
  putStrLn "--- Running Batch Delete Coupons Scenario ---"
  let eAppInst = mkSubscriptionAppInstance txHashStr txIndex
  case eAppInst of
    Left err -> putStrLn $ "❌ ERROR: Could not create App Instance: " ++ unpack err
    Right appInstance -> do
      putStrLn $ "✓ App Instance created: " ++ show (appInstanceBootstrapRef appInstance)

      -- 1. Manual setup (outside the harness)
      client <- loadModsefaClient "config.json" "keys/spender.skey" "sub-batch-delete-test"
      let clientEnv = mcEnv client

      -- 2. Query *BEFORE*
      putStrLn "Querying existing coupons to select a batch for deletion..."
      couponsBefore <- queryStateInstances (Proxy @CouponState) clientEnv appInstance
      let countBefore = length couponsBefore

      let mBatchIdToDelete = fmap (couponBatchId . siData) (listToMaybe couponsBefore)
      case mBatchIdToDelete of
        Nothing -> putStrLn "❌ ERROR: No existing coupons found to delete. Run coupon creation scenario first."
        Just batchIdToDelete -> do
          let couponsToDelete = [siData c | c <- couponsBefore, couponBatchId (siData c) == batchIdToDelete]
          let countInBatch = length couponsToDelete
          putStrLn $ "✓ Selected " ++ show countInBatch ++ " coupons from batch " ++ show batchIdToDelete ++ " for deletion."

          -- 3. Run the action
          let paramsTuple = couponsToDelete
          deleteResult <- runAction client appInstance (Proxy @BatchDeleteCouponsSpec) paramsTuple
          handleResult "BatchDeleteCoupons" deleteResult

          -- 4. Verify *AFTER*
          case deleteResult of
            Left _ -> putStrLn "Skipping verification due to action failure."
            Right _ -> do
              putStrLn "\n-- Verifying State Post-BatchDeleteCoupons --"
              putStrLn "(Waiting 5 seconds for chain index...)"
              threadDelay 5000000

              putStrLn "Querying CouponState again..."
              couponsAfter <- queryStateInstances (Proxy @CouponState) clientEnv appInstance
              let countAfter = length couponsAfter
              queryAndPrintState (const $ pure couponsAfter) id appInstance

              putStrLn "\nVerification Checks:"
              putStrLn $ "  - Coupons Before:   " ++ show countBefore
              putStrLn $ "  - Coupons in Batch: " ++ show countInBatch
              putStrLn $ "  - Coupons After:    " ++ show countAfter

              if countAfter == countBefore - countInBatch
                then putStrLn "✅ Verification: Coupon batch was successfully deleted."
                else putStrLn "❌ Verification: Coupon count is incorrect."

  putStrLn "\n--- Batch Delete Coupons Scenario Complete ---"

-- | Test scenario runner for subscribing a customer to a tier.
runSubscribeTest :: String -> Integer -> IO ()
runSubscribeTest txHashStr txIndex = do
  let eAppInst = mkSubscriptionAppInstance txHashStr txIndex
  case eAppInst of
    Left err -> putStrLn $ "❌ ERROR: Could not create App Instance: " ++ unpack err
    Right appInstance -> do

      let actionToRun client = do
            let (customerPkh, _) = head (mcSigners client)
            let tierNameToSubscribe = "Basic Tier" :: BuiltinByteString

            let paramsTuple = (customerPkh, tierNameToSubscribe)
            runAction client appInstance (Proxy @SubscribeSpec) paramsTuple

      let verify _ _ = return True -- Simple success check

      runScenario
        "Subscribe"
        "config.json"
        "keys/spender.skey"
        appInstance
        actionToRun
        verify

-- | Test scenario runner for subscribing a customer using a coupon.
-- (This test remains manual to check before/after state)
runSubscribeWithCouponTest :: String -> Integer -> IO ()
runSubscribeWithCouponTest txHashStr txIndex = do
  putStrLn "--- Running Subscribe With Coupon Scenario ---"
  let eAppInst = mkSubscriptionAppInstance txHashStr txIndex
  case eAppInst of
    Left err -> putStrLn $ "❌ ERROR: Could not create App Instance: " ++ unpack err
    Right appInstance -> do
      putStrLn $ "✓ App Instance created: " ++ show (appInstanceBootstrapRef appInstance)

      client <- loadModsefaClient "config.json" "keys/spender.skey" "sub-subscribe-coupon-test"
      let clientEnv = mcEnv client

      putStrLn "Querying for available coupons..."
      couponsBefore <- queryStateInstances (Proxy @CouponState) clientEnv appInstance
      let countBefore = length couponsBefore

      case listToMaybe couponsBefore of
        Nothing -> do
          putStrLn "❌ ERROR: No coupons found on-chain for this instance. Run batchCreateCoupons first."
        Just couponInstance -> do
          let couponToUse = siData couponInstance
          let couponCodeToUse = couponId couponToUse
          let couponBatchRefToUse = couponBatchId couponToUse
          putStrLn $ "✓ Found coupon to use: ID=" ++ show couponCodeToUse ++ ", BatchRef=" ++ show couponBatchRefToUse

          let (customerPkh, _) = head (mcSigners client)
          let tierNameToSubscribe = "Basic Tier" :: BuiltinByteString

          let paramsTuple = (customerPkh, tierNameToSubscribe, couponCodeToUse, couponBatchRefToUse)
          subscribeResult <- runAction client appInstance (Proxy @SubscribeWithCouponSpec) paramsTuple
          handleResult "SubscribeWithCoupon" subscribeResult

          case subscribeResult of
            Left _ -> putStrLn "Skipping verification due to action failure."
            Right _ -> do
              putStrLn "\n-- Verifying State Post-SubscribeWithCoupon --"
              putStrLn "(Waiting 5 seconds for chain index...)"
              threadDelay 5000000

              putStrLn "Querying remaining CouponState..."
              couponsAfter <- queryStateInstances (Proxy @CouponState) clientEnv appInstance
              let countAfter = length couponsAfter
              queryAndPrintState (const $ pure couponsAfter) id appInstance

              putStrLn "\nVerification Checks:"
              putStrLn $ "  - Coupons Before: " ++ show countBefore
              putStrLn $ "  - Coupons After:  " ++ show countAfter

              if countAfter == countBefore - 1
                then putStrLn "✅ Verification: One coupon was successfully consumed."
                else putStrLn "❌ Verification: Coupon count is incorrect."

  putStrLn "\n--- Subscribe With Coupon Scenario Complete ---"

-- | Test scenario runner for withdrawing funds from the treasury.
runWithdrawTreasuryTest :: String -> Integer -> IO ()
runWithdrawTreasuryTest txHashStr txIndex = do
  let eAppInst = mkSubscriptionAppInstance txHashStr txIndex
  case eAppInst of
    Left err -> putStrLn $ "❌ ERROR: Could not create App Instance: " ++ unpack err
    Right appInstance -> do
    
      -- Define the action to run
      let actionToRun client = do
            let amountToWithdraw = 10000000 :: Integer -- Withdraw 10 ADA
            let destinationAddress = addressToPlutus (wAddress (mcWallet client))

            let paramsTuple = (amountToWithdraw, destinationAddress)
            runAction client appInstance (Proxy @WithdrawTreasurySpec) paramsTuple

      let verify _ _ = do
            putStrLn "Verification: Check wallet manually to ensure funds arrived at destination."
            return True

      runScenario
        "WithdrawTreasury"
        "config.json"
        "keys/spender.skey"
        appInstance
        actionToRun
        verify

-- ============================================================================
-- * Negative Test Scenarios (Corruption)
-- ============================================================================

-- | Test scenario runner for submitting a corrupted InitializeServiceSpec transaction.
--   Attempts to mint a state token with an incorrect name.
--   Uses a specific bootstrap UTxO.
runSubscriptionCorruptedMintingTest :: String -> Integer -> IO ()
runSubscriptionCorruptedMintingTest txHashStr txIndex = do
  putStrLn "--- Running Corrupted Minting Scenario (InitializeService) ---"
  putStrLn $ "Using bootstrap UTxO: " ++ txHashStr ++ "#" ++ show txIndex

  -- 1. Create the SAppInstance for this test
  let eAppInst = mkSubscriptionAppInstance txHashStr txIndex
  case eAppInst of
    Left err -> putStrLn $ "❌ ERROR: Could not create App Instance: " ++ unpack err
    Right appInstance -> do
      putStrLn $ "✓ App Instance created: " ++ show (appInstanceBootstrapRef appInstance)

      -- 2. Use withClientEnv to get providers and network ID
      withClientEnv "config.json" "sub-corrupt-mint-test" $ \clientEnv -> do
        putStrLn "Client environment ready. Loading keys..."

        -- 3. Load the signing key (needed to sign the *attempt*)
        skey <- readSigningKey @'GYKeyRolePayment "keys/spender.skey"
        let ownAddress = addressFromPaymentKeyHash (ceNetworkId clientEnv) $
                           verificationKeyHash $ getVerificationKey skey

        -- 4. Define VALID parameters for the InitializeServiceSpec action
        providerPkh <- getPkhFromSKeyFile "keys/spender.skey"
        let serviceName    = "Corrupt Mint Service"
        let tierName       = "Corrupt Mint Tier"
        let price          = 1000000 :: Integer
        let assetClass     = assetClassToPlutus GYLovelace
        let billingPeriod  = 2592000000 :: POSIXTime
        let contractLength = 31536000000 :: POSIXTime
        let validParams = serviceName +: providerPkh +: tierName +: price +: assetClass +: billingPeriod +: contractLength +: End
        putStrLn "✓ Valid action parameters defined."

        -- 5. Define the CORRUPTION strategy (Targeting ServiceConfig token name)
        let strategy :: ActionMutationStrategy SubscriptionApp InitializeServiceSpec
            strategy = ActionMutationStrategy
              { mutations =
                  [ CorruptMintedTokenName
                      { -- Original name from ServiceConfigState
                        cmtnOriginalTokenName = "ServiceConfig"
                        -- Incorrect name
                      , cmtnCorruptedTokenName = "WrongConfigToken"
                      }
                  ]
              , description = "Attempt to mint ServiceConfig token with wrong name during init"
              }
        putStrLn $ "✓ Corruption strategy defined: " ++ unpack (description strategy)

        -- 6. Build the CORRUPTED transaction skeleton
        putStrLn "Building corrupted transaction skeleton..."
        eSkel <- buildRefCorruptedTransaction
                   appInstance
                   (Proxy @InitializeServiceSpec)
                   validParams
                   strategy
                   (ceNetworkId clientEnv)
                   (ceProviders clientEnv)

        case eSkel of
          Left err -> putStrLn $ "❌ Corrupted skeleton build failed unexpectedly: " ++ unpack err
          Right corruptedSkeleton -> do
            putStrLn "✅ Corrupted skeleton built successfully."
            pPrint corruptedSkeleton

            -- 7. Attempt to build, sign, and submit the corrupted transaction
            putStrLn "Attempting to build TxBody for corrupted skeleton..."
            eTxBody <- try $ runGYTxBuilderMonadIO
                         (ceNetworkId clientEnv)
                         (ceProviders clientEnv)
                         [ownAddress] ownAddress Nothing $
                         buildTxBody corruptedSkeleton

            case eTxBody of
              Left (e :: SomeException) ->
                putStrLn $ "✅ SUCCESS (Expected): TxBody build failed, indicating corruption caught early. Error: " ++ show e
              Right txBody -> do
                putStrLn "TxBody built successfully (corruption might be caught on-chain)."
                let signedTx = signGYTxBody txBody [skey]
                putStrLn "Transaction signed. Attempting submission (expected to fail)..."

                submitResult <- try $ gySubmitTx (ceProviders clientEnv) signedTx

                -- 8. Check the submission result
                case submitResult of
                  Left (e :: SomeException) ->
                    putStrLn $ "✅ SUCCESS (Expected): Transaction submission failed as expected. Error: " ++ show e
                  Right txId -> do
                    putStrLn "❌ FAILURE (Unexpected): Corrupted transaction submitted successfully!"
                    putStrLn $ "   Transaction ID: " ++ show txId
                    putStrLn "   This indicates a potential issue in the validator logic or the corruption strategy."

  putStrLn "\n--- Corrupted Minting Scenario Complete ---"

-- | Test scenario runner for submitting a corrupted SubscribeSpec transaction.
--   Attempts to set the customerSubscriptionPrice to 0, violating StateFieldValue.
--   Uses a specific bootstrap UTxO. Assumes subscriber uses "spender.skey".
runSubscriptionCorruptedSubscribeTest :: String -> Integer -> IO ()
runSubscriptionCorruptedSubscribeTest txHashStr txIndex = do
  putStrLn "--- Running Corrupted Subscribe Scenario (Price Calculation) ---"
  putStrLn $ "Using initialized instance from bootstrap UTxO: " ++ txHashStr ++ "#" ++ show txIndex

  -- 1. Create the SAppInstance for this test
  let eAppInst = mkSubscriptionAppInstance txHashStr txIndex
  case eAppInst of
    Left err -> putStrLn $ "❌ ERROR: Could not create App Instance: " ++ unpack err
    Right appInstance -> do
      putStrLn $ "✓ App Instance created: " ++ show (appInstanceBootstrapRef appInstance)

      -- 2. Use withClientEnv to get providers and network ID
      withClientEnv "config.json" "sub-corrupt-sub-test" $ \clientEnv -> do
        putStrLn "Client environment ready. Loading keys..."

        -- 3. Load the subscriber's signing key
        skey <- readSigningKey @'GYKeyRolePayment "keys/spender.skey"
        let ownAddress = addressFromPaymentKeyHash (ceNetworkId clientEnv) $
                           verificationKeyHash $ getVerificationKey skey

        -- 4. Define VALID parameters for the SubscribeSpec action
        customerPkh <- getPkhFromSKeyFile "keys/spender.skey"
        let tierNameToSubscribe = "Basic Tier"
        let validParams = customerPkh +: tierNameToSubscribe +: End
        putStrLn "✓ Valid action parameters defined."

        -- 5. Define the CORRUPTION strategy
        let strategy :: ActionMutationStrategy SubscriptionApp SubscribeSpec
            strategy = ActionMutationStrategy
              { mutations =
                  [ CorruptCalculatedField
                      (SStateType @CustomerSubscriptionState) -- Target CustomerSubscription outputs
                      -- This function corrupts the datum after it's correctly calculated
                      (\cs -> cs { customerSubscriptionPrice = 0 })
                  ]
              , description = "Attempt to subscribe but set price to 0 in output datum"
              }
        putStrLn $ "✓ Corruption strategy defined: " ++ unpack (description strategy)

        -- 6. Build the CORRUPTED transaction skeleton
        putStrLn "Building corrupted transaction skeleton..."
        eSkel <- buildRefCorruptedTransaction
                   appInstance
                   (Proxy @SubscribeSpec)
                   validParams
                   strategy
                   (ceNetworkId clientEnv)
                   (ceProviders clientEnv)

        case eSkel of
          Left err -> putStrLn $ "❌ Corrupted skeleton build failed unexpectedly: " ++ unpack err
          Right corruptedSkeleton -> do
            putStrLn "✅ Corrupted skeleton built successfully."
            pPrint corruptedSkeleton

            -- 7. Attempt to build, sign, and submit the corrupted transaction
            putStrLn "Attempting to build TxBody for corrupted skeleton..."
            eTxBody <- try $ runGYTxBuilderMonadIO
                         (ceNetworkId clientEnv)
                         (ceProviders clientEnv)
                         [ownAddress] ownAddress Nothing $
                         buildTxBody corruptedSkeleton

            case eTxBody of
              Left (e :: SomeException) ->
                putStrLn $ "✅ SUCCESS (Expected): TxBody build failed, indicating corruption caught early. Error: " ++ show e
              Right txBody -> do
                putStrLn "TxBody built successfully (corruption might be caught on-chain)."
                let signedTx = signGYTxBody txBody [skey]
                putStrLn "Transaction signed. Attempting submission (expected to fail)..."

                submitResult <- try $ gySubmitTx (ceProviders clientEnv) signedTx

                -- 8. Check the submission result
                case submitResult of
                  Left (e :: SomeException) ->
                    putStrLn $ "✅ SUCCESS (Expected): Transaction submission failed as expected. Error: " ++ show e
                  Right txId -> do
                    putStrLn "❌ FAILURE (Unexpected): Corrupted transaction submitted successfully!"
                    putStrLn $ "   Transaction ID: " ++ show txId
                    putStrLn "   This indicates a potential issue in the validator logic or the corruption strategy."

  putStrLn "\n--- Corrupted Subscribe Scenario Complete ---"

-- | Test scenario runner for submitting a corrupted SubscribeWithCouponSpec transaction.
--   Attempts to use a PricingTierState reference input from a different app instance.
--   Requires two distinct initialized instances (Instance A and Instance B).
runSubscriptionCorruptedInstanceRefTest
  :: String -> Integer -- ^ Bootstrap TxHash for Instance A
  -> String -> Integer -- ^ Bootstrap TxHash for Instance B
  -> IO ()
runSubscriptionCorruptedInstanceRefTest txHashA idxA txHashB idxB = do
  putStrLn "--- Running Corrupted Instance Reference Scenario (SubscribeWithCoupon) ---"
  putStrLn $ "Instance A Bootstrap: " ++ txHashA ++ "#" ++ show idxA
  putStrLn $ "Instance B Bootstrap: " ++ txHashB ++ "#" ++ show idxB

  let eAppInstA = mkSubscriptionAppInstance txHashA idxA
  let eAppInstB = mkSubscriptionAppInstance txHashB idxB

  case (eAppInstA, eAppInstB) of
    (Left err, _) -> putStrLn $ "❌ ERROR: Could not create App Instance A: " ++ unpack err
    (_, Left err) -> putStrLn $ "❌ ERROR: Could not create App Instance B: " ++ unpack err
    (Right appInstanceA, Right appInstanceB) -> do
      putStrLn $ "✓ App Instance A created: " ++ show (appInstanceBootstrapRef appInstanceA)
      putStrLn $ "✓ App Instance B created: " ++ show (appInstanceBootstrapRef appInstanceB)

      withClientEnv "config.json" "sub-corrupt-inst-test" $ \clientEnv -> do
        putStrLn "Client environment ready. Loading keys..."

        skey <- readSigningKey @'GYKeyRolePayment "keys/spender.skey"
        let ownAddress = addressFromPaymentKeyHash (ceNetworkId clientEnv) $
                           verificationKeyHash $ getVerificationKey skey

        putStrLn "Querying prerequisites..."
        mCouponInstA <- listToMaybe <$> queryStateInstances (Proxy @CouponState) clientEnv appInstanceA
        mTierInstB   <- listToMaybe <$> queryStateInstances (Proxy @PricingTierState) clientEnv appInstanceB

        case (mCouponInstA, mTierInstB) of
          (Nothing, _) -> putStrLn "❌ ERROR: Prerequisite failed - No coupon found for Instance A."
          (_, Nothing) -> putStrLn "❌ ERROR: Prerequisite failed - No PricingTier found for Instance B."
          (Just couponInstA, Just tierInstB) -> do
            let couponToUse = siData couponInstA
            let incorrectTierRefGY = siUtxoRef tierInstB
            let incorrectTierRefV3 = gyTxOutRefToV3 incorrectTierRefGY
            putStrLn $ "✓ Found coupon from Instance A: ID=" ++ show (couponId couponToUse)
            putStrLn $ "✓ Found incorrect PricingTier Ref from Instance B: " ++ show incorrectTierRefV3

            customerPkh <- getPkhFromSKeyFile "keys/spender.skey"
            let tierNameToSubscribe = "Basic Tier"
            let validParams = customerPkh
                           +: tierNameToSubscribe
                           +: couponId couponToUse
                           +: couponBatchId couponToUse -- Coupon's batch ref
                           +: End
            putStrLn "✓ Valid action parameters defined (relative to Instance A)."

            let strategy :: ActionMutationStrategy SubscriptionApp SubscribeWithCouponSpec
                strategy = ActionMutationStrategy
                  { mutations =
                      [ CorruptReferenceInput
                          (SStateType @PricingTierState)
                          incorrectTierRefV3
                      ]
                  , description = "Replace PricingTier ref input with one from different instance"
                  }
            putStrLn $ "✓ Corruption strategy defined: " ++ unpack (description strategy)

            putStrLn "Building corrupted transaction skeleton (targeting Instance A)..."
            eSkel <- buildRefCorruptedTransaction
                       appInstanceA
                       (Proxy @SubscribeWithCouponSpec)
                       validParams
                       strategy
                       (ceNetworkId clientEnv)
                       (ceProviders clientEnv)

            case eSkel of
              Left err -> putStrLn $ "❌ Corrupted skeleton build failed unexpectedly: " ++ unpack err
              Right corruptedSkeleton -> do
                putStrLn "✅ Corrupted skeleton built successfully."
                pPrint corruptedSkeleton

                putStrLn "Attempting to build TxBody for corrupted skeleton..."
                eTxBody <- try $ runGYTxBuilderMonadIO
                             (ceNetworkId clientEnv)
                             (ceProviders clientEnv)
                             [ownAddress] ownAddress Nothing $
                             buildTxBody corruptedSkeleton

                case eTxBody of
                  Left (e :: SomeException) ->
                    putStrLn $ "✅ SUCCESS (Expected): TxBody build failed, indicating corruption caught early. Error: " ++ show e
                  Right txBody -> do
                    putStrLn "TxBody built successfully (corruption should be caught on-chain)."
                    let signedTx = signGYTxBody txBody [skey]
                    putStrLn "Transaction signed. Attempting submission (expected to fail)..."
                    submitResult <- try $ gySubmitTx (ceProviders clientEnv) signedTx

                    case submitResult of
                      Left (e :: SomeException) ->
                        putStrLn $ "✅ SUCCESS (Expected): Transaction submission failed as expected. Error: " ++ show e
                      Right txId -> do
                        putStrLn "❌ FAILURE (Unexpected): Corrupted transaction submitted successfully!"
                        putStrLn $ "   Transaction ID: " ++ show txId
                        putStrLn "   This indicates a potential issue in the instance check logic."

  putStrLn "\n--- Corrupted Instance Reference Scenario Complete ---"

-- | Test scenario runner for submitting a corrupted SubscribeSpec transaction.
--   Attempts to pay an incorrect amount to the treasury.
--   Uses a specific bootstrap UTxO. Assumes subscriber uses "spender.skey".
runSubscriptionCorruptedPaymentTest :: String -> Integer -> IO ()
runSubscriptionCorruptedPaymentTest txHashStr txIndex = do
  putStrLn "--- Running Corrupted Payment Scenario (Subscribe) ---"
  putStrLn $ "Using initialized instance from bootstrap UTxO: " ++ txHashStr ++ "#" ++ show txIndex

  -- 1. Create the SAppInstance for this test
  let eAppInst = mkSubscriptionAppInstance txHashStr txIndex
  case eAppInst of
    Left err -> putStrLn $ "❌ ERROR: Could not create App Instance: " ++ unpack err
    Right appInstance -> do
      putStrLn $ "✓ App Instance created: " ++ show (appInstanceBootstrapRef appInstance)

      -- 2. Use withClientEnv to get providers and network ID
      withClientEnv "config.json" "sub-corrupt-pay-test" $ \clientEnv -> do
        putStrLn "Client environment ready. Loading keys..."

        -- 3. Load the subscriber's signing key
        skey <- readSigningKey @'GYKeyRolePayment "keys/spender.skey"
        let ownAddress = addressFromPaymentKeyHash (ceNetworkId clientEnv) $
                           verificationKeyHash $ getVerificationKey skey

        -- 4. Define VALID parameters for the SubscribeSpec action
        customerPkh <- getPkhFromSKeyFile "keys/spender.skey"
        let tierNameToSubscribe = "Basic Tier"
        let validParams = customerPkh +: tierNameToSubscribe +: End
        putStrLn "✓ Valid action parameters defined."

        -- 5. Define the CORRUPTION strategy
        let strategy :: ActionMutationStrategy SubscriptionApp SubscribeSpec
            strategy = ActionMutationStrategy
              { mutations =
                  [ CorruptConstraintOutput
                      (SStateType @TreasuryAdaState) -- Target the Treasury output
                      -- Function to replace the correct value with 1 lovelace
                      (\_ -> valueFromLovelace 1)
                  ]
              , description = "Attempt to subscribe but pay only 1 lovelace to treasury"
              }
        putStrLn $ "✓ Corruption strategy defined: " ++ unpack (description strategy)

        -- 6. Build the CORRUPTED transaction skeleton
        putStrLn "Building corrupted transaction skeleton..."
        eSkel <- buildRefCorruptedTransaction
                   appInstance
                   (Proxy @SubscribeSpec) -- Use SubscribeSpec
                   validParams
                   strategy
                   (ceNetworkId clientEnv)
                   (ceProviders clientEnv)

        case eSkel of
          Left err -> putStrLn $ "❌ Corrupted skeleton build failed unexpectedly: " ++ unpack err
          Right corruptedSkeleton -> do
            putStrLn "✅ Corrupted skeleton built successfully."
            pPrint corruptedSkeleton

            -- 7. Attempt to build, sign, and submit the corrupted transaction
            putStrLn "Attempting to build TxBody for corrupted skeleton..."
            eTxBody <- try $ runGYTxBuilderMonadIO
                         (ceNetworkId clientEnv)
                         (ceProviders clientEnv)
                         [ownAddress] ownAddress Nothing $
                         buildTxBody corruptedSkeleton

            case eTxBody of
              Left (e :: SomeException) ->
                putStrLn $ "✅ SUCCESS (Expected): TxBody build failed, indicating corruption caught early. Error: " ++ show e
              Right txBody -> do
                putStrLn "TxBody built successfully (corruption might be caught on-chain)."
                let signedTx = signGYTxBody txBody [skey]
                putStrLn "Transaction signed. Attempting submission (expected to fail)..."

                submitResult <- try $ gySubmitTx (ceProviders clientEnv) signedTx

                -- 8. Check the submission result
                case submitResult of
                  Left (e :: SomeException) ->
                    putStrLn $ "✅ SUCCESS (Expected): Transaction submission failed as expected. Error: " ++ show e
                  Right txId -> do
                    putStrLn "❌ FAILURE (Unexpected): Corrupted transaction submitted successfully!"
                    putStrLn $ "   Transaction ID: " ++ show txId
                    putStrLn "   This indicates a potential issue in the validator logic or the corruption strategy."

  putStrLn "\n--- Corrupted Payment Scenario Complete ---"

-- | Test scenario runner for submitting a corrupted UpdateServiceConfigSpec transaction.
--   Attempts to change the serviceConfigProvider even though it should be preserved.
--   Uses a specific bootstrap UTxO. Assumes provider uses "spender.skey".
runSubscriptionCorruptProviderUpdateTest :: String -> Integer -> IO ()
runSubscriptionCorruptProviderUpdateTest txHashStr txIndex = do
  putStrLn "--- Running Corrupted Provider Update Scenario (UpdateServiceConfig) ---"
  putStrLn $ "Using initialized instance from bootstrap UTxO: " ++ txHashStr ++ "#" ++ show txIndex

  -- 1. Create the SAppInstance for this test
  let eAppInst = mkSubscriptionAppInstance txHashStr txIndex
  case eAppInst of
    Left err -> putStrLn $ "❌ ERROR: Could not create App Instance: " ++ unpack err
    Right appInstance -> do
      putStrLn $ "✓ App Instance created: " ++ show (appInstanceBootstrapRef appInstance)

      -- 2. Use withClientEnv
      withClientEnv "config.json" "sub-corrupt-prov-test" $ \clientEnv -> do
        putStrLn "Client environment ready. Loading keys..."

        -- 3. Load the current provider's signing key
        skey <- readSigningKey @'GYKeyRolePayment "keys/spender.skey"
        let ownAddress = addressFromPaymentKeyHash (ceNetworkId clientEnv) $
                           verificationKeyHash $ getVerificationKey skey

        -- 4. Define VALID parameters for the UpdateServiceConfigSpec action
        let validNewName = "Name During Corrupt Provider Test"
        let validParams = validNewName +: End
        putStrLn "✓ Valid action parameters defined."

        -- 5. Define the CORRUPTION strategy
        -- Generate a dummy PKH for corruption
        -- In a real test, you might load a second key or use a known invalid hash.
        let corruptPkh = "00112233445566778899aabbccddeeff00112233445566778899aabb" :: PubKeyHash
        let strategy :: ActionMutationStrategy SubscriptionApp UpdateServiceConfigSpec
            strategy = ActionMutationStrategy
              { mutations =
                  [ CorruptPreservedFieldInRef
                      (STypedTheOnlyInstance (SStateType @ServiceConfigState)) -- Target the unique ServiceConfig state
                      (Proxy @"serviceConfigProvider") -- Target the field that should be preserved
                      (toData corruptPkh) -- Corrupting value
                  ]
              , description = "Attempt to change provider PKH during config name update"
              }
        putStrLn $ "✓ Corruption strategy defined: " ++ unpack (description strategy)

        -- 6. Build the CORRUPTED transaction skeleton
        putStrLn "Building corrupted transaction skeleton..."
        eSkel <- buildRefCorruptedTransaction
                   appInstance
                   (Proxy @UpdateServiceConfigSpec) -- Use UpdateServiceConfigSpec
                   validParams
                   strategy
                   (ceNetworkId clientEnv)
                   (ceProviders clientEnv)

        case eSkel of
          Left err -> putStrLn $ "❌ Corrupted skeleton build failed unexpectedly: " ++ unpack err
          Right corruptedSkeleton -> do
            putStrLn "✅ Corrupted skeleton built successfully."
            pPrint corruptedSkeleton

            -- 7. Attempt to build, sign, and submit the corrupted transaction
            putStrLn "Attempting to build TxBody for corrupted skeleton..."
            eTxBody <- try $ runGYTxBuilderMonadIO
                         (ceNetworkId clientEnv)
                         (ceProviders clientEnv)
                         [ownAddress] ownAddress Nothing $
                         buildTxBody corruptedSkeleton

            case eTxBody of
              Left (e :: SomeException) ->
                putStrLn $ "✅ SUCCESS (Expected): TxBody build failed, indicating corruption caught early. Error: " ++ show e
              Right txBody -> do
                putStrLn "TxBody built successfully (corruption might be caught on-chain)."
                let signedTx = signGYTxBody txBody [skey] -- Sign with *current* provider key
                putStrLn "Transaction signed. Attempting submission (expected to fail)..."

                submitResult <- try $ gySubmitTx (ceProviders clientEnv) signedTx

                -- 8. Check the submission result
                case submitResult of
                  Left (e :: SomeException) ->
                    putStrLn $ "✅ SUCCESS (Expected): Transaction submission failed as expected. Error: " ++ show e
                  Right txId -> do
                    putStrLn "❌ FAILURE (Unexpected): Corrupted transaction submitted successfully!"
                    putStrLn $ "   Transaction ID: " ++ show txId
                    putStrLn "   This indicates a potential issue in the validator logic or the corruption strategy."

  putStrLn "\n--- Corrupted Provider Update Scenario Complete ---"

-- ============================================================================
-- * Diagnostic Utilities
-- ============================================================================

-- | Compiles and prints the Intermediate Representation (IR) for the SubscriptionApp.
--   This is a developer utility for inspecting the generated structure.
runSubscriptionIR :: IO ()
runSubscriptionIR = do
  putStrLn "=== Compiling SubscriptionApp to IR ==="
  let appSpec' = autoSingletonFull @SubscriptionApp
      appName = "SubscriptionApp" -- App name for IR root
      appIR = compileIR appName appSpec'
  putStrLn "✅ Compilation to IR successful!"
  putStrLn "📊 Pretty-printing the AppIR:"
  pPrint appIR