{-|
Module      : Modsefa.Client.TransactionRunner
Description : High-level functions for building, signing, and submitting Modsefa actions.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC (requires FlexibleContexts, GADTs, RankNTypes, etc.)

This module provides the core functions to execute Modsefa actions.
It provides two main ways to run an action:

1.  'runAction': The preferred, generic function that takes action parameters
    as a standard Haskell tuple. This is the primary entry point for
    most client code.
2.  'runModsefaAction': A lower-level alternative that takes parameters as
    the GADT 'SParamTuple'. 'runAction' is a wrapper around this.

It also exports the building blocks for more complex dApp workflows (like
browser-based signing):

* 'buildModsefaTxBody':   Builds the unsigned, balanced transaction.
* 'signModsefaTxBody':    Signs a transaction body with all available local keys.
* 'submitModsefaTx':      Submits a fully signed transaction.

Finally, it provides a convenience helper, 'loadModsefaClient', for setting up
a 'ModsefaClient' handle for tests and scripts.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Modsefa.Client.TransactionRunner
  ( runModsefaAction
  , buildModsefaTxBody
  , signModsefaTxBody
  , submitModsefaTx
  , loadModsefaClient
  , runAction
  ) where

import Control.Exception (SomeException, try)
import Data.Proxy (Proxy(..))
import Data.Text (Text, pack, unpack)
import Data.Typeable (Typeable)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Text.Pretty.Simple (pPrint)

import GeniusYield.TxBuilder
  ( buildTxBody, runGYTxBuilderMonadIO
  )
import GeniusYield.Types
  ( GYSomeSigningKey(..), GYTx, GYTxBody, GYTxId, gySubmitTx, signGYTxBody'
  )
import PlutusLedgerApi.V3 (PubKeyHash)

import Modsefa.Core.Foundation
  ( ActionSpecName, ActionSpecParameters, ActionSpecSteps, ActionTransitions
  , AllStateTypesGeneric, AppInstanceParameters, AppSpec(..)
  , ExtractConstraintsFromAction, ExtractOpsFromAction, ExtractPlutusVersion
  , ExtractStateTypes, InitialAppState, ParameterDerivations, ParamsToValue
  , ResolveInstanceParamList
  )
import Modsefa.Core.Singletons
  ( AutoSingletonActionSpec, AutoSingletonActionTransitionList
  , AutoSingletonAppInstanceParams, AutoSingletonAppStateList
  , AutoSingletonParamDerivationList, AutoSingletonValidators, SAppInstance
  , SParamTuple
  )
import Modsefa.Core.Transaction
  ( ActionConstraintsValid, ProcessActionSteps, buildTransactionDirect
  )
import Modsefa.Core.ValidatorScript (AppValidatorScripts)

import Modsefa.Client.ActionParams (ToSParamTuple (toSParamTuple))
import Modsefa.Client.Types (ClientEnv(..), ModsefaClient(..), withClientEnv)
import Modsefa.Client.Wallet (SigningMethod(..), Wallet(..), loadWalletAndSigner)


-- | Builds, signs, and submits a transaction for a specified Modsefa action
--   in a single step.
runModsefaAction :: forall app action params.
                    ( AppSpec app
                    , KnownSymbol (InitialAppState app)
                    , AutoSingletonValidators (Validators app)
                    , AutoSingletonAppStateList (AppStates app)
                    , AutoSingletonActionTransitionList (ActionTransitions app)
                    , AutoSingletonAppInstanceParams (AppInstanceParameters app)
                    , AutoSingletonActionSpec action
                    , ActionSpecParameters action ~ params
                    , KnownSymbol (ActionSpecName action)
                    , AllStateTypesGeneric (ExtractStateTypes (ExtractOpsFromAction action))
                    , AppValidatorScripts app
                    , Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
                    , ActionConstraintsValid (ExtractConstraintsFromAction action)
                    , ProcessActionSteps app action (ActionSpecSteps action) params (ExtractPlutusVersion app)
                    , AutoSingletonParamDerivationList (ParameterDerivations app)
                    )
                 => ClientEnv            -- ^ Client environment (providers, network).
                 -> SAppInstance app     -- ^ The specific application instance being interacted with.
                 -> Proxy action         -- ^ Proxy identifying the action.
                 -> SParamTuple params   -- ^ Action parameters.
                 -> Wallet               -- ^ The *primary* wallet for balancing, change, and collateral.
                 -> [(PubKeyHash, SigningMethod)] -- ^ A list of *all* available signers for this TX.
                 -> IO (Either Text GYTxId) -- ^ Result: Error text or TxId in IO.
runModsefaAction clientEnv appInstance actionProxy params wallet allSigners = do
  -- 1. Build the Transaction Body
  eTxBody <- buildModsefaTxBody clientEnv appInstance actionProxy params wallet
  case eTxBody of
    Left err -> return $ Left err
    Right txBody -> do
      -- 2. Sign the Transaction Body
      eSignedTx <- signModsefaTxBody txBody allSigners
      case eSignedTx of
        Left err -> return $ Left err
        Right signedTx -> do
          -- 3. Submit the Signed Transaction
          submitModsefaTx clientEnv signedTx

-- | Step 1 (Build): Creates the balanced, unsigned transaction body.
buildModsefaTxBody :: forall app action params.
                    (
                      AppSpec app
                    , KnownSymbol (InitialAppState app)
                    , AutoSingletonValidators (Validators app)
                    , AutoSingletonAppStateList (AppStates app)
                    , AutoSingletonActionTransitionList (ActionTransitions app)
                    , AutoSingletonAppInstanceParams (AppInstanceParameters app)
                    , AutoSingletonActionSpec action
                    , ActionSpecParameters action ~ params
                    , KnownSymbol (ActionSpecName action)
                    , AllStateTypesGeneric (ExtractStateTypes (ExtractOpsFromAction action))
                    , AppValidatorScripts app
                    , Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
                    , ActionConstraintsValid (ExtractConstraintsFromAction action)
                    , ProcessActionSteps app action (ActionSpecSteps action) params (ExtractPlutusVersion app)
                    , AutoSingletonParamDerivationList (ParameterDerivations app)
                    )
                 => ClientEnv            -- ^ Client environment (providers, network).
                 -> SAppInstance app     -- ^ The specific application instance.
                 -> Proxy action         -- ^ Proxy identifying the action.
                 -> SParamTuple params   -- ^ Action parameters.
                 -> Wallet               -- ^ The wallet providing balancing UTxOs, change address, and collateral.
                 -> IO (Either Text GYTxBody)
buildModsefaTxBody clientEnv appInstance actionProxy params wallet = do
  let networkId   = ceNetworkId clientEnv
  let providers   = ceProviders clientEnv
  let ownAddrs    = wBalanceUtxos wallet
  let changeAddr  = wAddress wallet
  let mCollateral = wCollateral wallet

  -- 1. Build Skeleton
  putStrLn $ "Building skeleton for action: " ++ symbolVal (Proxy @(ActionSpecName action))
  -- Note: buildTransactionDirect returns a GYTxSkeleton v,
  -- but buildTxBody consumes it and produces a non-parameterized GYTxBody
  skeletonResult <- buildTransactionDirect appInstance actionProxy params networkId providers
  case skeletonResult of
    Left buildErr -> do
      putStrLn $ "Skeleton build failed: " ++ unpack buildErr
      return $ Left buildErr
    Right skeleton -> do
      putStrLn "Skeleton built successfully."
      pPrint skeleton

      -- 2. Build TxBody
      putStrLn "Building transaction body..."
      let collateralTuple = fmap (, False) mCollateral -- Use False for non-pure collateral
      eTxBody <- try $ runGYTxBuilderMonadIO networkId providers ownAddrs changeAddr collateralTuple $
                   buildTxBody skeleton
      case eTxBody of
        Left (e :: SomeException) -> do
          let errMsg = "TxBody build failed: " <> pack (show e)
          putStrLn $ unpack errMsg
          return $ Left errMsg
        Right txBody -> do
          putStrLn "Transaction body built successfully."
          return $ Right txBody

-- | Step 2 (Sign): Signs a transaction body with all available signing methods.
--   This function signs with *all* provided keys, similar to GY's 'signGYTxBody',
--   to ensure that key-witnessed inputs (like a bootstrap UTxO) are signed
--   even if they are not explicitly listed in txBodyReqSignatories.
signModsefaTxBody
  :: GYTxBody
  -> [(PubKeyHash, SigningMethod)]    -- ^ A list of *all* available signers.
  -> IO (Either Text GYTx)
signModsefaTxBody txBody allSigners = do
  -- 1. Extract all available GYSomeSigningKey from the SigningMethod list.
  let availableSKeys = extractSKeys allSigners
  putStrLn $ "Signing transaction with " ++ show (length availableSKeys) ++ " available key(s)..."

  -- 2. Sign the transaction body with all available keys.
  let signedTx = signGYTxBody' txBody availableSKeys
  putStrLn "Transaction signed."
  return $ Right signedTx
  
  where
    -- Helper to extract GYSomeSigningKey from our SigningMethod wrapper
    extractSKeys :: [(PubKeyHash, SigningMethod)] -> [GYSomeSigningKey]
    extractSKeys [] = []
    extractSKeys ((_, SignWithSKey sk) : rest) = GYSomeSigningKey sk : extractSKeys rest
    -- Add cases for other SigningMethods (e.g., browser) here if they
    -- can provide a GYSomeSigningKey. Browser/hardware wallets will be
    -- handled differently (in the two-endpoint flow).

-- | Step 3 (Submit): Submits a fully signed transaction to the node.
submitModsefaTx
  :: ClientEnv
  -> GYTx
  -> IO (Either Text GYTxId)
submitModsefaTx clientEnv signedTx = do
  putStrLn "Submitting transaction..."
  eTxId <- try $ gySubmitTx (ceProviders clientEnv) signedTx
  case eTxId of
    Left (e :: SomeException) -> do
      let errMsg = "Submission failed: " <> pack (show e)
      putStrLn $ unpack errMsg
      return $ Left errMsg
    Right txId -> do
      putStrLn $ "Transaction submitted successfully: " ++ show txId
      return $ Right txId

-- | A helper to create a simple ModsefaClient from a single skey file.
--   This is great for tests and scripts.
loadModsefaClient
  :: FilePath           -- ^ Path to config.json
  -> FilePath           -- ^ Path to the user's signing key file
  -> Text               -- ^ Log namespace
  -> IO ModsefaClient
loadModsefaClient configPath skeyPath logLabel =
  withClientEnv configPath logLabel $ \clientEnv -> do
    (wallet, signer) <- loadWalletAndSigner skeyPath (ceNetworkId clientEnv)
    return $ ModsefaClient
      { mcEnv     = clientEnv
      , mcWallet  = wallet
      , mcSigners = [signer]
      }

-- | A generic, type-safe function for executing any Modsefa action
--   using a standard Haskell tuple for parameters.
runAction :: forall app action params tuple.
             ( AppSpec app
             , KnownSymbol (InitialAppState app)
             , AutoSingletonValidators (Validators app)
             , AutoSingletonAppStateList (AppStates app)
             , AutoSingletonActionTransitionList (ActionTransitions app)
             , AutoSingletonAppInstanceParams (AppInstanceParameters app)
             , AutoSingletonActionSpec action
             , ActionSpecParameters action ~ params
             , KnownSymbol (ActionSpecName action)
             , AllStateTypesGeneric (ExtractStateTypes (ExtractOpsFromAction action))
             , AppValidatorScripts app
             , Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
             , ActionConstraintsValid (ExtractConstraintsFromAction action)
             , ProcessActionSteps app action (ActionSpecSteps action) params (ExtractPlutusVersion app)
             , AutoSingletonParamDerivationList (ParameterDerivations app)
             , ToSParamTuple tuple params
             )
          => ModsefaClient        -- ^ The client session handle.
          -> SAppInstance app     -- ^ The specific application instance.
          -> Proxy action         -- ^ A proxy identifying the action.
          -> tuple                -- ^ The action parameters as a standard Haskell tuple.
          -> IO (Either Text GYTxId)
runAction client appInstance actionProxy paramsTuple =
  -- 1. Convert the Haskell tuple into the SParamTuple GADT
  let paramsSParamTuple = toSParamTuple paramsTuple
  -- 2. Call the original runModsefaAction function
  in runModsefaAction
       (mcEnv client)
       appInstance
       actionProxy
       paramsSParamTuple
       (mcWallet client)
       (mcSigners client)