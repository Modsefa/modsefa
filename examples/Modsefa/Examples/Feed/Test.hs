{-|
Module      : Modsefa.Examples.Feed.Test
Description : Example test functions for the Modsefa Feed application.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module provides entry points ('IO' actions) for running predefined test
scenarios against the Feed example application ('FeedApp'). These scenarios
utilize the client functions defined in "Modsefa.Examples.Feed.Client" to
execute actions like initialization and updates. It also includes negative
tests that use the corruption framework ('Modsefa.Test.RefAwareMutation') to
verify validator robustness.

Scenarios typically require network configuration ("config.json"),
signing keys ("spender.skey"), and a bootstrap 'TxOutRef' hash/index
(provided as arguments) to identify the application instance on a testnet.
It also includes a developer utility ('runFeedIR') for inspecting the
generated Intermediate Representation.
-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Modsefa.Examples.Feed.Test
  ( -- * Test Scenario Runners
    runFeedInitTest
  , runFeedUpdateTest
    -- * Negative Test Scenarios (Corruption)
  , runFeedUpdateCorruptionTest
    -- * Developer Utilities
  , runFeedIR
  ) where

import Control.Exception (try)
import Data.Maybe (isJust, listToMaybe, maybeToList)
import Data.Proxy (Proxy(Proxy))
import Data.Text (unpack)
import GHC.Exception (SomeException)
import Text.Pretty.Simple (pPrint)

import GeniusYield.TxBuilder (buildTxBody, runGYTxBuilderMonadIO)
import GeniusYield.Types
  ( GYKeyRole(GYKeyRolePayment), addressFromPaymentKeyHash, getVerificationKey
  , gySubmitTx, readSigningKey, signGYTxBody, verificationKeyHash
  )
import PlutusLedgerApi.V3 (BuiltinByteString, toData)

import Modsefa.Client
  ( ClientEnv(..), ModsefaClient(..), queryAndPrintState, queryStateInstances
  , runAction, withClientEnv, (+:), pattern End
  )
import Modsefa.Core.IR.Compiler (compileIR)
import Modsefa.Core.Singletons (autoSingletonFull, autoSingletonStateSpec)
import Modsefa.Test.Harness (runScenario)
import Modsefa.Test.RefAwareMutation
  ( ActionMutation(CorruptConstantField), ActionMutationStrategy(..)
  , buildRefCorruptedTransaction
  )

import Modsefa.Examples.Feed.Client (appInstanceBootstrapRef, mkFeedAppInstance)
import Modsefa.Examples.Feed.Scripts ()
import Modsefa.Examples.Feed.Spec (FeedApp, InitializeFeedSpec, UpdateFeedSpec)
import Modsefa.Examples.Feed.Types 
  ( FeedConfigState, FeedDataState, FeedStatus(Active)
  )


-- ============================================================================
-- * Test Scenario Runners
-- ============================================================================

-- | Test scenario runner for initializing the FeedApp using a specific bootstrap UTxO.
runFeedInitTest :: String -> Integer -> IO ()
runFeedInitTest txHashStr txIndex = do
  let eAppInst = mkFeedAppInstance txHashStr txIndex
  case eAppInst of
    Left err -> putStrLn $ "❌ ERROR: Could not create App Instance: " ++ unpack err
    Right appInstance -> do

      -- Define the action to run
      let actionToRun client = do
            let (ownerPkh, _) = head (mcSigners client) -- Get owner from client
            let feedName = "Test Init Feed" :: BuiltinByteString
            let initialContent = "Content from initialization test." :: BuiltinByteString
            -- Define the parameters as a simple Haskell tuple
            let paramsTuple = (feedName, ownerPkh, initialContent)

            -- Call the generic runAction
            runAction client appInstance (Proxy @InitializeFeedSpec) paramsTuple

      let verify clientEnv appInst = do
            -- Call generic queryStateInstances
            mConfig <- listToMaybe <$> queryStateInstances (Proxy @FeedConfigState) clientEnv appInstance
            dataStates <- queryStateInstances (Proxy @FeedDataState) clientEnv appInstance

            -- Call generic queryAndPrintState
            putStrLn "Querying FeedConfigState..."
            queryAndPrintState (const $ pure mConfig) maybeToList appInst
            putStrLn "Querying FeedDataState..."
            queryAndPrintState (const $ pure dataStates) id appInst

            return (isJust mConfig && not (null dataStates))

      -- Run the scenario
      runScenario
        "InitializeFeed"
        "config.json"
        "keys/spender.skey"
        appInstance
        actionToRun
        verify

-- | Test scenario runner for updating an existing FeedApp instance.
--   Assumes the instance identified by the bootstrap UTxO has been initialized.
runFeedUpdateTest :: String -> Integer -> IO ()
runFeedUpdateTest txHashStr txIndex = do
  let eAppInst = mkFeedAppInstance txHashStr txIndex
  case eAppInst of
    Left err -> putStrLn $ "❌ ERROR: Could not create App Instance: " ++ unpack err
    Right appInstance -> do

      let actionToRun client = do
            let newContent = "Updated content from scenario." :: BuiltinByteString
            -- This uses the "Single item" instance
            let paramsTuple = newContent
            runAction client appInstance (Proxy @UpdateFeedSpec) paramsTuple

      -- Define the verification check
      let verify clientEnv appInst = do
            -- Call generic queryStateInstances
            dataStates <- queryStateInstances (Proxy @FeedDataState) clientEnv appInstance

            -- Call generic queryAndPrintState
            putStrLn "Querying FeedDataState..."
            queryAndPrintState (const $ pure dataStates) id appInst

            return (length dataStates >= 2)

      runScenario
        "UpdateFeed"
        "config.json"
        "keys/spender.skey"
        appInstance
        actionToRun
        verify

-- ============================================================================
-- * Negative Test Scenarios (Corruption)
-- ============================================================================

-- | Test scenario runner for submitting a corrupted UpdateFeedSpec transaction.
--   Attempts to set the archived state's status back to Active, expecting failure.
--   Assumes the instance identified by the bootstrap UTxO has been initialized.
runFeedUpdateCorruptionTest :: String -> Integer -> IO ()
runFeedUpdateCorruptionTest txHashStr txIndex = do
  putStrLn "--- Running Feed Update Corruption Scenario ---"
  putStrLn $ "Using initialized instance from bootstrap UTxO: " ++ txHashStr ++ "#" ++ show txIndex

  -- 1. Create the SAppInstance for the existing instance
  let eAppInst = mkFeedAppInstance txHashStr txIndex
  case eAppInst of
    Left err -> putStrLn $ "❌ ERROR: Could not create App Instance: " ++ unpack err
    Right appInstance -> do
      putStrLn $ "✓ App Instance created: " ++ show (appInstanceBootstrapRef appInstance)

      -- 2. Use withClientEnv to get providers and network ID
      withClientEnv "config.json" "feed-update-corrupt-test" $ \clientEnv -> do
        putStrLn "Client environment ready. Loading keys..."

        -- 3. Load the owner's signing key (needed to sign the *attempt*)
        skey <- readSigningKey @'GYKeyRolePayment "keys/spender.skey"
        let ownAddress = addressFromPaymentKeyHash (ceNetworkId clientEnv) $
                           verificationKeyHash $ getVerificationKey skey

        -- 4. Define VALID parameters for the UpdateFeedSpec action
        let validNewContent = "Content for corruption test."
        let validParams = validNewContent +: End
        putStrLn "✓ Valid action parameters defined."

        -- 5. Define the CORRUPTION strategy
        let strategy :: ActionMutationStrategy FeedApp UpdateFeedSpec
            strategy = ActionMutationStrategy
              { mutations =
                  [ CorruptConstantField
                      (autoSingletonStateSpec @FeedDataState)      -- Target FeedDataState outputs
                      (Proxy @"feedStatus")            -- Target the 'feedStatus' field
                      -- Try to set it to Active instead of Archived
                      (toData Active)
                  ]
              , description = "Attempt to archive a feed entry but set its status back to Active"
              }
        putStrLn $ "✓ Corruption strategy defined: " ++ unpack (description strategy)

        -- 6. Build the CORRUPTED transaction skeleton
        putStrLn "Building corrupted transaction skeleton..."
        eSkel <- buildRefCorruptedTransaction
                   appInstance
                   (Proxy @UpdateFeedSpec)
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
                -- Building the body might fail if corruption is severe (e.g., negative value)
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

  putStrLn "\n--- Feed Update Corruption Scenario Complete ---"

-- ============================================================================
-- * Diagnostic Utilities
-- ============================================================================

-- | Compiles and prints the Intermediate Representation (IR) for the FeedApp.
--   This is a developer utility for inspecting the generated structure.
runFeedIR :: IO ()
runFeedIR = do
  putStrLn "=== Compiling FeedApp to IR ==="
  let appSpec' = autoSingletonFull @FeedApp
      appName = "FeedApp" -- App name for IR root
      appIR = compileIR appName appSpec'
  putStrLn "✅ Compilation to IR successful!"
  putStrLn "📊 Pretty-printing the AppIR:"
  pPrint appIR