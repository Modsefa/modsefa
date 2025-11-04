{-|
Module      : Modsefa.Client.Types
Description : Client-side types and functions for querying Modsefa application instances.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC (requires GADTs, DataKinds, TypeFamilies, UndecidableInstances, etc.)

This module defines types and functions used by client applications to interact
with and query the on-chain state of Modsefa application instances.

Provides types for representing the client environment ('ClientEnv'),
found state instances ('StateInstance'), and functions for setting up
the environment ('withClientEnv') and querying state ('queryStateInstances').
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Modsefa.Client.Types
  ( -- * Client Environment Setup
    ClientEnv(..)
  , withClientEnv
    -- * Client Handle
  , ModsefaClient(..)
    -- * State Representation
  , StateInstance(..)
    -- * Query Functions
  , queryStateInstances
  ) where

import Control.Exception (SomeException, try)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy(Proxy))
import Data.Type.Bool (If)
import Data.Maybe (catMaybes)
import Data.String (fromString)
import Data.Text (Text, pack, unpack)
import Data.Time (UTCTime, getCurrentTime)
import GHC.Stack (HasCallStack)
import GHC.TypeLits (ErrorMessage(..), TypeError, symbolVal)

import GeniusYield.GYConfig
  ( GYCoreConfig (cfgNetworkId), coreConfigIO, withCfgProviders
  )
import GeniusYield.TxBuilder (GYTxQueryMonad(..), runGYTxQueryMonadIO, utxoDatum)
import GeniusYield.Types
    ( GYAddress, GYNetworkId, GYProviders, GYTxOutRef, GYUTxO(utxoValue, utxoRef)
    , GYValue, utxosToList )
import PlutusLedgerApi.V3 (FromData, PubKeyHash)

import Modsefa.Core.Foundation
  ( AppSpec(Validators), GetStateData, GetStateName, InstanceType(SingleInstance)
  , StateRepresentable, StateType, ValidatorDef(..)
  , ValidatorSpec(ManagedStates, ValidatorAppName, ValidatorInstanceType)
  )
import Modsefa.Core.Singletons
  ( SAppInstance(SAppInstance), SAppSpec(..)
  , SInstanceType(SMultiInstance, SSingleInstance), SStateList(..), SValidator(..)
  , SValidatorList(..), SomeStateType(..), SomeValidator(..)
  , findValidatorManagingState, getStateName
  )
import Modsefa.Core.Transaction (getValidatorAddressFromInstance)
import Modsefa.Core.ValidatorScript (AppValidatorScripts)

import Modsefa.Client.Wallet (SigningMethod, Wallet)


-- ============================================================================
-- * Client Environment Setup
-- ============================================================================

-- | A context holding common resources needed for interacting with the blockchain
--   via Modsefa client functions.
data ClientEnv = ClientEnv
    { ceProviders :: !GYProviders -- ^ Blockchain connection providers.
    , ceNetworkId :: !GYNetworkId -- ^ Network ID.
    }

-- | Sets up the 'ClientEnv' (providers, networkId) by loading configuration
--   and runs a given IO action within that context.
--
--   Handles resource management for providers using 'withCfgProviders'.
--
--   Example:
--   @
--   withClientEnv "config.json" "my-app" $ \clientEnv -> do
--     -- Use clientEnv (ceProviders, ceNetworkId)
--     putStrLn $ "Using network: " ++ show (ceNetworkId clientEnv)
--   @
withClientEnv :: (HasCallStack)
              => FilePath -- ^ Path to the core config JSON file (e.g., "config.json").
              -> Text     -- ^ A label for logging within providers.
              -> (ClientEnv -> IO a) -- ^ The action to run, receiving the environment.
              -> IO a
withClientEnv configPath logLabel action = do
    -- Load config to get networkId and provider settings
    coreCfg <- coreConfigIO configPath
    let networkId' = cfgNetworkId coreCfg -- Extract NetworkId from config

    -- Set up providers within their managed scope
    let namespace = fromString (unpack logLabel)
    withCfgProviders coreCfg namespace $ \providers -> do
        -- Create the ClientEnv record
        let env = ClientEnv
              { ceProviders   = providers
              , ceNetworkId   = networkId'
              }
        -- Execute the user's action with the created environment
        action env

-- ============================================================================
-- * Client Handle
-- ============================================================================

-- | A handle bundling all necessary client context into a single object.
--   This represents a user's "session".
data ModsefaClient = ModsefaClient
  { mcEnv     :: !ClientEnv
  , mcWallet  :: !Wallet
  , mcSigners :: ![(PubKeyHash, SigningMethod)]
  }

-- ============================================================================
-- * State Representation
-- ============================================================================

-- | Represents a concrete instance of a specific 'StateType' found on the blockchain.
-- It bundles the UTxO reference, the parsed datum, the value held in the UTxO,
-- and a timestamp indicating when the query occurred.
data StateInstance (st :: StateType) = StateInstance
  { siUtxoRef :: GYTxOutRef -- ^ The 'GYTxOutRef' identifying the UTxO holding this state instance.
  , siData :: GetStateData st -- ^ The actual parsed Haskell datum value (e.g., 'ServiceConfig').
  , siValue :: GYValue -- ^ The 'GYValue' contained within the UTxO (assets and amounts).
  , siTimestamp :: UTCTime -- ^ Timestamp indicating when this state instance was queried.
  }

-- | Show instance for 'StateInstance'.
instance (Show (GetStateData st)) => Show (StateInstance st) where
  show (StateInstance ref dat val time) =
    "StateInstance { siUtxoRef = " <> show ref <>
    ", siData = " <> show dat <>
    ", siValue = " <> show val <>
    ", siTimestamp = " <> show time <> " }"

-- | Eq instance for 'StateInstance'.
instance (Eq (GetStateData st)) => Eq (StateInstance st) where
  (StateInstance ref1 dat1 val1 time1) == (StateInstance ref2 dat2 val2 time2) =
    ref1 == ref2 && dat1 == dat2 && val1 == val2 && time1 == time2

-- | Existential wrapper for 'StateInstance'. Allows storing instances of different
-- 'StateType's together, hiding the specific type @st@ while preserving the
-- 'StateRepresentable' constraint.
data SomeStateInstance where
  SomeStateInstance :: StateRepresentable st => StateInstance st -> SomeStateInstance

-- | Show instance for 'SomeStateInstance'. Shows minimal identifying information.
instance Show SomeStateInstance where
  show (SomeStateInstance (StateInstance ref _ _ time)) =
    "SomeStateInstance(" <> show ref <> " at " <> show time <> ")"

-- ============================================================================
-- Type Family for State-in-App Validation
-- ============================================================================

-- | Type-level constraint: Checks if a 'StateType' @st@ is managed by any validator
-- within the 'AppSpec' @app@. Produces a 'TypeError' if the state is not found.
-- Used in the signature of 'queryStateInstances' to provide compile-time safety.
type family StateInApp (st :: StateType) (app :: Type) :: Constraint where
  StateInApp st app = StateInAppValidators st (Validators app)

-- | (Internal) Helper for 'StateInApp': Recursively checks the list of 'ValidatorDef's.
type family StateInAppValidators (st :: StateType) (validators :: [ValidatorDef]) :: Constraint where
  StateInAppValidators st '[] = TypeError ('Text "State " ':<>: 'ShowType st ':<>: 'Text " not found in any validator's ManagedStates within the AppSpec.")
  StateInAppValidators st ('Validator v ': rest) =
    If (StateInValidatorStates st (ManagedStates v))
       (() :: Constraint)
       (StateInAppValidators st rest)

-- | (Internal) Helper for 'StateInAppValidators': Checks if a 'StateType' @st@ exists in a list of states.
type family StateInValidatorStates (st :: StateType) (states :: [StateType]) :: Bool where
  StateInValidatorStates st '[] = 'False
  StateInValidatorStates st (st ': rest) = 'True
  StateInValidatorStates st (other ': rest) = StateInValidatorStates st rest

-- ============================================================================
-- * Query Functions
-- ============================================================================

-- | Retrieves the script address ('GYAddress') for a specific single-instance validator
-- within the application instance.
-- Uses 'getValidatorAddressFromInstance' internally.
getValidatorAddressTyped :: forall v app.
                          ( ValidatorSpec v
                          , ValidatorInstanceType v ~ 'SingleInstance
                          , AppValidatorScripts app
                          )
                       => Proxy v -- ^ Proxy identifying the validator type.
                       -> ClientEnv -- ^ The client environment.
                       -> SAppInstance app -- ^ The query context.
                       -> IO (Either Text GYAddress) -- ^ 'Right GYAddress' or 'Left error'.
getValidatorAddressTyped _proxy clientEnv appInstance = do
  let validatorName = pack $ symbolVal (Proxy @(ValidatorAppName v))
  -- Delegate to the core transaction utility function.
  getValidatorAddressFromInstance validatorName appInstance (ceNetworkId clientEnv) (ceProviders clientEnv)

-- | Retrieves all UTxOs currently sitting at the address of a specific single-instance validator.
getValidatorUtxos :: forall v app.
                   ( ValidatorSpec v
                   , ValidatorInstanceType v ~ 'SingleInstance
                   , AppValidatorScripts app
                   )
                => Proxy v -- ^ Proxy identifying the validator type.
                -> ClientEnv -- ^ The client environment.
                -> SAppInstance app -- ^ The query context.
                -> IO (Either Text [GYUTxO]) -- ^ 'Right' list of UTxOs or 'Left error'.
getValidatorUtxos proxy clientEnv appInstance = do
  -- First, get the validator's address.
  addressResult <- getValidatorAddressTyped proxy clientEnv appInstance

  case addressResult of
    Left err -> return $ Left err
    Right validatorAddress -> do
      -- Query all UTxOs at that address using Genius Yield's query monad.
      let networkId' = ceNetworkId clientEnv
      let providers = ceProviders clientEnv
      utxosResult <- try $ runGYTxQueryMonadIO networkId' providers $
                           utxosAtAddress validatorAddress Nothing

      case utxosResult of
        Left (ex :: SomeException) ->
          return $ Left $ "Failed to query UTxOs: " <> pack (show ex)
        Right utxos ->
          return $ Right $ utxosToList utxos

-- | Parses a list of 'GYUTxO's, attempting to decode their datums into the specified
-- 'StateType' @st@. Returns a list of successfully parsed 'StateInstance's.
parseStateInstances :: forall st. (StateRepresentable st, FromData (GetStateData st))
                   => Proxy st -- ^ Proxy for the target 'StateType'.
                   -> [GYUTxO] -- ^ List of UTxOs to parse.
                   -> GYProviders
                   -> GYNetworkId
                   -> IO [StateInstance st] -- ^ List of successfully parsed instances.
parseStateInstances _proxy utxos providers networkId' = do
  currentTime <- getCurrentTime

  -- Use GYTxQueryMonad to handle potential datum lookups (though here we focus on inline).
  results <- runGYTxQueryMonadIO networkId' providers $
    mapM (parseUtxoInQuery currentTime) utxos

  return $ catMaybes results
  where
    -- Helper to parse a single UTxO within the query monad.
    parseUtxoInQuery currentTime utxo = do
      -- Attempt to get the datum (handles inline/hash lookup implicitly).
      datumResult <- utxoDatum utxo
      case datumResult of
        Left _err -> return Nothing
        Right (_addr, _val, stateData) -> return $ Just $ StateInstance
          { siUtxoRef = utxoRef utxo
          , siData = stateData
          , siValue = utxoValue utxo
          , siTimestamp = currentTime
          }

-- | (Internal) Finds the 'SomeValidator' singleton managing a state identified by its name ('Text').
findValidatorManagingStateName :: SAppSpec app -> Text -> Maybe SomeValidator
findValidatorManagingStateName appSpec' stateTypeName =
  -- First, find the SomeStateType corresponding to the name.
  case findStateTypeByName appSpec' stateTypeName of
    Just someStateType ->
      -- Then, find the validator managing that state type.
      case findValidatorManagingState appSpec' someStateType of
        Left _err -> Nothing
        Right someValidator -> Just someValidator
    Nothing -> Nothing

-- | (Internal) Finds the 'SomeStateType' existential wrapper corresponding to a state name ('Text')
-- within an 'SAppSpec'. Searches through all managed states of all validators.
findStateTypeByName :: SAppSpec app -> Text -> Maybe SomeStateType
findStateTypeByName (SAppSpec validators _ _ _ _ _) targetName =
  searchInValidators validators
  where
    -- Search through the list of validators.
    searchInValidators :: SValidatorList vs -> Maybe SomeStateType
    searchInValidators SVNil = Nothing
    searchInValidators (SVCons validator rest) =
      case searchInValidator validator of
        Just found -> Just found
        Nothing -> searchInValidators rest

    -- Search through the managed states of a single validator.
    searchInValidator :: forall v. ValidatorSpec v => SValidator v -> Maybe SomeStateType
    searchInValidator (SValidator _ managedStates _ _) =
      searchInStateList managedStates

    -- Search through a list of state types.
    searchInStateList :: SStateList sts -> Maybe SomeStateType
    searchInStateList SSNil = Nothing
    searchInStateList (SSCons stateType rest) =
      if getStateName stateType == targetName
      then Just (SomeStateType stateType)
      else searchInStateList rest

-- | (Internal) Retrieves all UTxOs for a validator specified by the 'SomeValidator' existential wrapper.
-- Currently only supports single-instance validators.
getUtxosFromSomeValidator :: forall app. (AppValidatorScripts app) =>
                           SomeValidator -> -- ^ The existential validator wrapper.
                           ClientEnv -> -- ^ The client environment.
                           SAppInstance app -> -- ^ Query context.
                           IO [GYUTxO] -- ^ List of UTxOs at the validator's address.
getUtxosFromSomeValidator (SomeValidator (validator :: SValidator v)) clientEnv appInstance = do
  -- Check the instance type of the validator.
  case validator of
    SValidator _ _ _ SSingleInstance -> do
      -- Only proceed if it's a single-instance validator.
      let proxy = Proxy @v
      utxosResult <- getValidatorUtxos proxy clientEnv appInstance
      case utxosResult of
        Left _err -> return []
        Right utxos -> return utxos
    SValidator _ _ _ SMultiInstance -> do
      -- Skip multi-instance validators for now (address depends on parameters).
      -- TODO: Implement multi-instance UTXO querying if needed.
      return []

-- | Queries the blockchain for all instances of a specific 'StateType' @st@ belonging
-- to a given application instance.
--
-- This function:
-- 1. Finds the validator responsible for managing the state type @st@.
-- 2. Determines the on-chain address for that validator (currently only supports single-instance validators).
-- 3. Queries all UTxOs at that address.
-- 4. Attempts to parse the datum of each UTxO as the Haskell type corresponding to @st@ ('GetStateData st').
-- 5. Returns a list of 'StateInstance's for all successfully parsed UTxOs.
--
-- The 'StateInApp' constraint ensures at compile time that the requested state type
-- is actually part of the specified application, preventing invalid queries.
queryStateInstances :: forall st app.
                     ( StateRepresentable st -- Constraint: State must be representable on-chain.
                     , FromData (GetStateData st) -- Constraint: Datum type must be deserializable.
                     , AppSpec app -- Constraint: App type must have an AppSpec.
                     , AppValidatorScripts app -- Constraint: App must provide script implementations.
                     , StateInApp st app -- Constraint: State must belong to the app (compile-time check).
                     )
                  => Proxy st -- ^ Proxy indicating the 'StateType' to query for.
                  -> ClientEnv -- ^ The client environment.
                  -> SAppInstance app -- ^ The application instance query context.
                  -> IO [StateInstance st] -- ^ List of found and parsed state instances.
queryStateInstances proxy clientEnv appInstance = do
  let SAppInstance appSpec' _instanceParams = appInstance
  let networkId' = ceNetworkId clientEnv
  let providers  = ceProviders clientEnv
  -- Get the name of the state type from the proxy.
  let stateTypeName = pack $ symbolVal (Proxy @(GetStateName st))

  -- Find the validator managing this state by name.
  case findValidatorManagingStateName appSpec' stateTypeName of
    Nothing -> return []  
    Just someValidator -> do
      -- Get all UTxOs managed by this validator.
      utxos <- getUtxosFromSomeValidator someValidator clientEnv appInstance
      -- Parse the UTxOs into StateInstances of the target type 'st'.
      parseStateInstances proxy utxos providers networkId'