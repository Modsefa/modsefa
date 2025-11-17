{-|
Module      : Modsefa.Test.RefAwareMutation
Description : Utilities for testing Modsefa validators by corrupting transaction skeletons.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC (requires GADTs, DataKinds, TypeFamilies, etc.)

This module provides tools for negative testing of Modsefa validator scripts.
It defines types for specifying targeted corruptions ('ActionMutation', 'ActionMutationStrategy')
to a valid transaction skeleton ('GeniusYield.TxBuilder.GYTxSkeleton'). The main function,
'buildRefCorruptedTransaction', first builds a valid skeleton for a given Modsefa action
and then applies the specified corruptions, resulting in a skeleton that is expected
to fail on-chain validation. This helps verify that the validator logic correctly
rejects invalid state transitions.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Modsefa.Test.RefAwareMutation
  ( -- * Mutation Types
    ActionMutation (..)
  , ActionMutationStrategy (..)
    -- * Main Corruption Function
  , buildRefCorruptedTransaction
  ) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.State (evalStateT)
import Data.Kind (Type)
import Data.List (partition)
import Data.Map (Map, delete, empty, fromList, insert, lookup, toList)
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy)
import Data.Set (Set, delete, insert, toList)
import Data.Text (Text, pack, unpack)
import Data.Typeable (Typeable)
import GHC.Generics (Generic(Rep))
import GHC.TypeLits (KnownSymbol, symbolVal)

import GeniusYield.TxBuilder
  ( GYTxQueryMonad(utxoAtTxOutRef), GYTxSkeleton(..)
  , GYTxSkeletonRefIns(GYTxSkeletonRefIns), runGYTxQueryMonadIO
  )
import GeniusYield.Types
  ( GYAddress, GYAssetClass(GYToken), GYBuildScript, GYDatum, GYMintingPolicyId
  , GYNetworkId, GYOutDatum(GYOutDatumInline), GYProviders, GYRedeemer, GYTokenName
  , GYTxOut(gyTxOutAddress, gyTxOutDatum, gyTxOutValue), GYTxOutRef
  , GYUTxO(utxoAddress, utxoValue, utxoOutDatum), GYValue, SingPlutusVersionI
  , datumFromPlutus', mintingPolicyId, mintingPolicyIdFromWitness
  , mintingPolicyIdToCurrencySymbol, txOutRefFromPlutus, valueFromList
  , valueFromLovelace, valueSplitAda, valueToList
  )
import PlutusLedgerApi.V1 (TxId(TxId), TxOutRef(TxOutRef))
import PlutusLedgerApi.V3
  ( CurrencySymbol, Data(..), FromData(..), ToData(..), TxId(TxId)
  , TxOutRef(TxOutRef)
  )

import Modsefa.Core.Foundation
  ( ActionSpecName, ActionSpecParameters, ActionSpecSteps, AllStateTypesGeneric
  , AppSpec(AppInstanceParameters, ParameterDerivations, Validators)
  , ExtractConstraintsFromAction, ExtractOpsFromAction, ExtractPlutusVersion
  , ExtractPlutusVersionFromValidators, ExtractStateTypes, GExtractField
  , ParamsToValue, ResolveInstanceParamList, SomeFieldValue(SomeFieldValue)
  , StateDatum, StateDatumConstraints, StateSpec, TypedActionSpec, TypedStateRef
  )
import Modsefa.Core.Singletons
  ( AutoSingletonActionSpec(..), AutoSingletonParamDerivationList, SActionSpec(..)
  , SActionStep(SMap, SOp), SActionStepList(..), SAppInstance(SAppInstance)
  , SAppSpec, SOperation(SCreate), SParamTuple(STupleNil), SStateRef(..)
  , SStateSpec(SStateSpec), SomeStateType(SomeStateType)
  , SomeValidator(SomeValidator), extractFieldFromDatum, extractStateFromRef
  , findValidatorManagingState, getOwnPolicyTokenDetails, getStateName
  , getValidatorNameFromSingleton
  )
import Modsefa.Core.Transaction
  ( ActionConstraintsValid, ProcessActionSteps, SomeValidatorWithScript(..)
  , TxBuilder, TxBuilderContext(..), buildTransactionDirect, findValidatorWithScript
  , getValidatorAddressFromInstance, resolveStateRefSingleton
  , resolveValidatorParametersFromAllSources, runTxBuilder
  )
import Modsefa.Core.ValidatorScript (AppValidatorScripts, getValidatorScript)

import Modsefa.Test.GenericCorruption (GCorruptField, corruptFieldGenerically)


-- ============================================================================
-- * Core Mutation Types
-- ============================================================================

-- | Defines specific ways to corrupt a transaction skeleton for negative testing.
-- Each constructor targets a different aspect of the transaction (datums, minting, references)
-- based on the structure of the Modsefa action specification.
data ActionMutation (app :: Type) (action :: TypedActionSpec) where
  -- | Corrupts a field within an output datum that corresponds to a state instance
  -- identified by an 'SStateRef', specifically targeting a field that should have been
  -- preserved ('Modsefa.Core.Foundation.Types.Preserve') during an update operation.
  CorruptPreservedFieldInRef ::
    forall (app :: Type) (action :: TypedActionSpec) s (ref :: TypedStateRef s) field.
    ( StateSpec s
    , Generic (StateDatum s)
    , GCorruptField (Rep (StateDatum s))
    , GExtractField (Rep (StateDatum s))
    , StateDatumConstraints s
    , KnownSymbol field
    ) =>
    SStateRef s ref -> -- ^ Reference to the state instance being updated.
    Proxy field -> -- ^ Proxy for the field name ('Symbol') that should be preserved.
    Data -> -- ^ The incorrect Plutus 'Data' to replace the field's value with.
    ActionMutation app action

  -- | Corrupts the token name used in a minting operation ('gytxMint') and the corresponding
  -- output ('gytxOuts') carrying the minted token. Finds the token based on the validator's
  -- 'OwnPolicy' and the state name.
  CorruptMintedTokenName ::
    { -- | The original, correct token name ('GYTokenName') expected in the minting field and output value.
      cmtnOriginalTokenName :: GYTokenName
    , -- | The new, incorrect token name ('GYTokenName') to replace the original with.
      cmtnCorruptedTokenName :: GYTokenName
    } -> ActionMutation app action

  -- | Corrupts a field within an output datum that is expected to be set to a constant
  -- value ('Modsefa.Core.Foundation.Types.SetTo' with 'EnumValue' or 'IntValue') during a create or update.
  CorruptConstantField ::
    forall (app :: Type) (action :: TypedActionSpec) s field.
    ( StateSpec s
    , Generic (StateDatum s)
    , GCorruptField (Rep (StateDatum s))
    , GExtractField (Rep (StateDatum s))
    , StateDatumConstraints s
    , KnownSymbol field
    ) =>
    SStateSpec s -> -- ^ Singleton representing the state type of the output datum.
    Proxy field -> -- ^ Proxy for the field name ('Symbol') being set to a constant.
    Data -> -- ^ The incorrect Plutus 'Data' to replace the field's value with.
    ActionMutation app action

  -- | Corrupts a field within an output datum whose value is calculated based on other inputs
  -- ('Modsefa.Core.Foundation.Types.SetTo' with 'StateFieldValue', 'CurrentTime', arithmetic operations).
  -- Uses a provided function to modify the correctly calculated datum value.
  CorruptCalculatedField ::
    forall (app :: Type) (action :: TypedActionSpec) s.
    ( StateSpec s
    , Generic (StateDatum s)
    , ToData (StateDatum s)
    , StateDatumConstraints s
    ) =>
    SStateSpec s -> -- ^ Singleton representing the state spec of the output datum.
    (StateDatum s -> StateDatum s) -> -- ^ A function that takes the valid datum and returns a corrupted version.
    ActionMutation app action

  -- | Corrupts a reference input ('gytxRefIns') in the transaction skeleton.
  -- Finds the reference input corresponding to the given state type and replaces it with an incorrect 'TxOutRef'.
  CorruptReferenceInput ::
    forall (app :: Type) (action :: TypedActionSpec) s.
    ( StateSpec s
    , StateDatumConstraints s
    ) =>
    SStateSpec s -> -- ^ State type of the reference input UTxO to find and replace.
    PlutusLedgerApi.V3.TxOutRef -> -- ^ The incorrect 'TxOutRef' (Plutus V3) to substitute.
    ActionMutation app action

  -- | Corrupts the value ('GYValue') of an output generated specifically to satisfy a constraint
  -- (e.g., 'Modsefa.Core.Foundation.Types.MustAddToAggregateState'). Finds the output by looking for the one sent to the
  -- managing validator's address for the specified aggregate state.
  CorruptConstraintOutput ::
    forall (app :: Type) (action :: TypedActionSpec) s.
    ( StateSpec s
    , StateDatumConstraints s
    ) =>
    SStateSpec s -> -- ^ The aggregate state type whose constraint-generated output should be corrupted.
    (GYValue -> GYValue) -> -- ^ A function to modify the output's 'GYValue' (e.g., reduce the amount).
    ActionMutation app action

-- | Show instance for 'ActionMutation' for debugging purposes. Hides potentially large 'Data' values.
instance Show (ActionMutation app action) where
  show (CorruptPreservedFieldInRef stateRef fieldProxy _corruptionValue) =
    "CorruptPreservedFieldInRef(" ++ show stateRef ++ "." ++ symbolVal fieldProxy ++ ", <corruption_value>)"
  show (CorruptMintedTokenName o c) =
    "CorruptMintedTokenName { original = " ++ show o ++ ", corrupted = " ++ show c ++ " }"
  show (CorruptConstantField stateTypeProxy fieldProxy _corruptionValue) =
    "CorruptConstantField(" ++ show stateTypeProxy ++ "." ++ symbolVal fieldProxy ++ ", <corruption_value>)"
  show (CorruptCalculatedField stateTypeProxy _) =
    "CorruptCalculatedField(" ++ show stateTypeProxy ++ ", <corruption_function>)"
  show (CorruptReferenceInput stype incorrectRef) =
    "CorruptReferenceInput(Find: " ++ show stype ++ ", ReplaceWith: " ++ show incorrectRef ++ ")"
  show (CorruptConstraintOutput stype _) =
    "CorruptConstraintOutput(" ++ show stype ++ ", <corruption_function>)"

-- | Defines a strategy for corrupting a transaction, consisting of a list of
-- specific 'ActionMutation's to apply and a descriptive text.
data ActionMutationStrategy app action = ActionMutationStrategy
  { mutations :: [ActionMutation app action] -- ^ List of corruptions to apply sequentially.
  , description :: Text -- ^ A human-readable description of the corruption strategy's goal.
  }

-- | Show instance for 'ActionMutationStrategy'.
instance Show (ActionMutationStrategy app action) where
  show (ActionMutationStrategy mutations' desc) =
    show desc ++ ": " ++ show mutations'

-- ============================================================================
-- * Main Corruption Function
-- ============================================================================

-- | Builds a potentially corrupted transaction skeleton for negative testing.
--
-- 1.  Builds a valid 'GYTxSkeleton' for the given action and parameters using 'buildTransactionDirect'.
-- 2.  If successful, applies each 'ActionMutation' specified in the 'ActionMutationStrategy' sequentially
--     to the valid skeleton.
-- 3.  Returns the final (potentially corrupted) skeleton or an error if either the initial build
--     or any mutation step fails.
--
-- This function is the main entry point for generating transaction skeletons intended to fail
-- validator checks on-chain.
--
-- @param appInstance The specific application instance ('SAppInstance').
-- @param actionProxy Proxy indicating the 'TypedActionSpec' to build and corrupt.
-- @param params Value-level tuple ('SParamTuple') of action parameters.
-- @param strategy The 'ActionMutationStrategy' defining the corruptions to apply.
-- @param networkId The target Cardano network ID.
-- @param providers Providers for blockchain interaction ('GYProviders').
-- @return 'IO' action returning either an error message ('Left Text') or the corrupted skeleton ('Right GYTxSkeleton').
buildRefCorruptedTransaction ::
  forall app action.
  ( AutoSingletonActionSpec action
  , KnownSymbol (ActionSpecName action)
  , AllStateTypesGeneric (ExtractStateTypes (ExtractOpsFromAction action))
  , AppValidatorScripts app
  , AppSpec app
  , Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
  , ActionConstraintsValid (ExtractConstraintsFromAction action)
  , SingPlutusVersionI (ExtractPlutusVersionFromValidators (Validators app))
  , ProcessActionSteps app action (ActionSpecSteps action) (ActionSpecParameters action) (ExtractPlutusVersion app)
  , AutoSingletonParamDerivationList (ParameterDerivations app)
  ) =>
  SAppInstance app ->
  Proxy action ->
  SParamTuple (ActionSpecParameters action) ->
  ActionMutationStrategy app action ->
  GYNetworkId ->
  GYProviders ->
  IO (Either Text (GYTxSkeleton (ExtractPlutusVersion app)))
buildRefCorruptedTransaction appInstance actionProxy params strategy networkId providers = do
  putStrLn $ "Building corrupted transaction with strategy: " ++ show strategy

  -- Step 1: Build valid transaction first
  putStrLn "Step 1: Building valid transaction..."
  validResult <- buildTransactionDirect appInstance actionProxy params networkId providers

  case validResult of
    Left err -> do
      putStrLn $ "Failed to build valid transaction: " ++ show err
      return $ Left $ "Failed to build valid transaction: " <> err
    Right validSkeleton -> do
      putStrLn "✅ Valid transaction built successfully"

      -- Step 2: Apply mutations
      putStrLn "Step 2: Applying mutations..."
      putStrLn $ "Number of mutations to apply: " ++ show (length $ mutations strategy)

      -- Apply each mutation sequentially within the TxBuilder monad.
      let initialContext = TxBuilderContext { tbcCurrentTime = 0, tbcLetResults = empty, tbcResolvedRefs = empty, tbcResolvedFields = empty }
      evalStateT (runTxBuilder (foldM (applyMutation appInstance networkId providers) (Right validSkeleton) (mutations strategy))) initialContext

-- ============================================================================
-- * Internal Helpers & Corruption Logic
-- ============================================================================

-- | (Internal) Applies a single 'ActionMutation' to a transaction skeleton within the 'TxBuilder' monad.
-- Dispatches to specific corruption logic based on the mutation constructor.
applyMutation ::
  forall app action v.
  ( AppSpec app
  , AppValidatorScripts app
  , AutoSingletonActionSpec action
  , Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
  , SingPlutusVersionI v
  ) =>
  SAppInstance app ->
  GYNetworkId ->
  GYProviders ->
  Either Text (GYTxSkeleton v) -> -- ^ The skeleton from the previous step (or initial valid skeleton).
  ActionMutation app action -> -- ^ The mutation to apply.
  TxBuilder v (Either Text (GYTxSkeleton v)) -- ^ The resulting skeleton or error.
applyMutation _appInstance _networkId _providers (Left err) _mutation =
  -- If previous step failed, propagate the error.
  return $ Left err
applyMutation appInstance networkId providers (Right skeleton) mutation = do
  -- Dispatch based on the mutation type.
  case mutation of
    CorruptPreservedFieldInRef stateRef fieldProxy corruptionValue -> do
      liftIO $ putStrLn $ "🔧 Corrupting preserved field: " ++ symbolVal fieldProxy
      -- Pass @s explicitly to corruptFieldInStateRef
      corruptFieldInStateRef stateRef fieldProxy skeleton appInstance networkId providers corruptionValue
    
    CorruptMintedTokenName originalName corruptedName -> do
      let (SAppInstance appSpec _) = appInstance
      let actionSpec = autoSingletonActionSpec :: SActionSpec action
      -- Find the validator responsible for minting this token based on OwnPolicy.
      case findMintingValidatorForToken appSpec actionSpec originalName of
        Nothing -> return $ Left $ "Could not find a validator that mints a token named '" <> pack (show originalName) <> "' with its OwnPolicy in this action."
        Just (SomeValidatorWithScript proxy validator) -> do
          -- Resolve validator parameters to get the script.
          let eRes = resolveValidatorParametersFromAllSources validator appInstance Nothing STupleNil []
          case eRes of
            Left err -> return $ Left ("Failed to resolve validator params: " <> err)
            Right validatorParams -> do
              let script = getValidatorScript @app proxy validatorParams
              let gyPolicyId = mintingPolicyId script
              let policyId = mintingPolicyIdToCurrencySymbol gyPolicyId

              -- Step 1: Corrupt the gytxMint field in the skeleton.
              let originalMint = gytxMint skeleton
              let eCorruptedMint = Data.Map.fromList <$> traverse (corruptMintEntry policyId originalName corruptedName) (Data.Map.toList originalMint)

              case eCorruptedMint of
                Left err -> return $ Left err
                Right corruptedMintMap -> do

                  -- Step 2: Corrupt the corresponding token name in the gytxOuts field.
                  let originalOutputs = gytxOuts skeleton
                  let corruptedOutputs = map (corruptOutputTokenName gyPolicyId originalName corruptedName) originalOutputs
                  let finalSkeleton = skeleton { gytxMint = corruptedMintMap, gytxOuts = corruptedOutputs }

                  return $ Right finalSkeleton

    CorruptConstantField (stateSpecProxy :: SStateSpec s) fieldProxy corruptionValue -> do
      liftIO $ putStrLn $ "🔧 Corrupting constant field: " ++ symbolVal fieldProxy ++ " in state " ++ show stateSpecProxy
      -- Find all outputs matching the target state type.
      let (matchingOutputs, otherOutputs) = partition (isOutputOfStateType' stateSpecProxy) (gytxOuts skeleton)
      
      if null matchingOutputs
        then return $ Left $ "No output found for state type '" <> pack (show stateSpecProxy) <> "' to corrupt."
        else do
          -- Corrupt the field in *all* matching outputs.
          corruptedOutputs <- liftIO $ mapM (\output ->
            corruptFieldInOutputWithStateType
              output
              fieldProxy
              stateSpecProxy
              corruptionValue
            ) matchingOutputs
          
          let newOutputs = otherOutputs ++ corruptedOutputs
          let corruptedSkeleton = skeleton { gytxOuts = newOutputs }
          return $ Right corruptedSkeleton

    CorruptCalculatedField (stateSpecProxy :: SStateSpec s) corruptionFunc -> do
      liftIO $ putStrLn $ "🔧 Corrupting calculated field in state " ++ show stateSpecProxy
      -- Find all outputs matching the target state type.
      let (matchingOutputs, otherOutputs) = partition (isOutputOfStateType' stateSpecProxy) (gytxOuts skeleton)
  
      case matchingOutputs of
        [] -> return $ Left $ "No output found for state type '" <> pack (show stateSpecProxy) <> "' to corrupt."
        (outputToCorrupt : restMatching) -> do -- Corrupt only the first match for simplicity.
          let mCorruptedOutput = applyCorruptionToOutput outputToCorrupt
          case mCorruptedOutput of
            Nothing -> return $ Left "Failed to decode or re-encode datum during corruption."
            Just corruptedOutput -> do
              let newOutputs = otherOutputs ++ [corruptedOutput] ++ restMatching
              let corruptedSkeleton = skeleton { gytxOuts = newOutputs }
              return $ Right corruptedSkeleton
      where
        -- Helper to apply the corruption function to an output's datum.
        applyCorruptionToOutput :: GYTxOut v -> Maybe (GYTxOut v)
        applyCorruptionToOutput output = do
          (originalDatum, datumType) <- gyTxOutDatum output
          let bd = toBuiltinData originalDatum
          originalStateData <- fromBuiltinData bd :: Maybe (StateDatum s) -- Decode datum
          let corruptedStateData = corruptionFunc originalStateData -- Apply user function
          let corruptedBuiltinData = toBuiltinData corruptedStateData -- Re-encode
          Just $ output { gyTxOutDatum = Just (datumFromPlutus' corruptedBuiltinData, datumType) }

    CorruptReferenceInput (stateSpecProxy :: SStateSpec s) incorrectTxOutRefPlutusV3 -> do
      liftIO $ putStrLn $ "🔧 Corrupting reference input for state type: " ++ show stateSpecProxy
      -- Convert the incorrect V3 TxOutRef to GY TxOutRef format.
      let PlutusLedgerApi.V3.TxOutRef (PlutusLedgerApi.V3.TxId txIdBS) txIx = incorrectTxOutRefPlutusV3
      let incorrectTxOutRefPlutusV1 = PlutusLedgerApi.V1.TxOutRef (PlutusLedgerApi.V1.TxId txIdBS) txIx
      let eIncorrectTxOutRefGY = txOutRefFromPlutus incorrectTxOutRefPlutusV1

      case eIncorrectTxOutRefGY of
        Left err -> return $ Left $ "Failed to convert incorrect TxOutRef: " <> pack (show err)
        Right incorrectRef -> do
          -- Get the original set of reference inputs from the skeleton.
          let originalRefs = case gytxRefIns skeleton of GYTxSkeletonRefIns refs -> refs; _ -> mempty

          -- Find the original reference input UTxO Ref matching the state type.
          eOriginalRef <- liftIO $ findOriginalRefInput originalRefs stateSpecProxy networkId providers

          case eOriginalRef of
            Right originalRef -> do
              liftIO $ putStrLn $ "   Found original ref input: " ++ show originalRef
              liftIO $ putStrLn $ "   Replacing with incorrect ref: " ++ show incorrectRef
            Left _ -> do
              liftIO $ putStrLn $ "   Original ref input not found for state type. Adding incorrect ref: " ++ show incorrectRef

          let newRefs = case eOriginalRef of
                Right originalRef ->
                  -- Replace: Insert incorrect, Delete original
                  Data.Set.insert incorrectRef (Data.Set.delete originalRef originalRefs)
                Left _ ->
                  -- Original not found, just add the incorrect one
                  Data.Set.insert incorrectRef originalRefs

          let corruptedSkeleton = skeleton { gytxRefIns = GYTxSkeletonRefIns newRefs }
          return $ Right corruptedSkeleton

    CorruptConstraintOutput (stateSpecProxy :: SStateSpec s) corruptionFunc -> do
      liftIO $ putStrLn $ "🔧 Corrupting constraint output value for state: " ++ show stateSpecProxy
      let (SAppInstance appSpec' _) = appInstance

      -- Find the validator that manages the target aggregate state.
      case findValidatorManagingState appSpec' (SomeStateType stateSpecProxy) of
        Left err -> return $ Left $ "Could not find validator for aggregate state: " <> err
        Right (SomeValidator validator) -> do
          -- Get that validator's on-chain address.
          let validatorName = getValidatorNameFromSingleton validator
          eAddress <- liftIO $ getValidatorAddressFromInstance validatorName appInstance networkId providers
          case eAddress of
            Left err -> return $ Left $ "Failed to get address for validator '" <> validatorName <> "': " <> err
            Right targetAddress -> do
              -- Find the output(s) in the skeleton sent to this address.
              let originalOutputs = gytxOuts skeleton
              let (matchingOutputs, otherOutputs) = partition (\o -> gyTxOutAddress o == targetAddress) originalOutputs

              if null matchingOutputs
              then return $ Left "Negative test setup failed: Could not find the constraint-generated output to corrupt."
              else do
                -- Corrupt the value of the first matching output.
                let outputToCorrupt = head matchingOutputs
                let corruptedValue = corruptionFunc (gyTxOutValue outputToCorrupt)
                let corruptedOutput = outputToCorrupt { gyTxOutValue = corruptedValue }
                -- Rebuild the skeleton with the corrupted output.
                let newOutputs = corruptedOutput : tail matchingOutputs ++ otherOutputs
                let corruptedSkeleton = skeleton { gytxOuts = newOutputs }
                return $ Right corruptedSkeleton

-- | (Internal) Corrupts a specific field within an output datum corresponding to a resolved state reference.
-- Uses 'findOutputsMatchingStateRef' to locate the correct output and 'corruptFieldInOutputWithStateType'.
corruptFieldInStateRef ::
  forall s (ref :: TypedStateRef s) field v app.
  ( StateSpec s
  , Generic (StateDatum s)
  , GCorruptField (Rep (StateDatum s))
  , GExtractField (Rep (StateDatum s))
  , KnownSymbol field
  , AppSpec app
  , AppValidatorScripts app
  , StateDatumConstraints s
  , Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
  , SingPlutusVersionI v
  ) =>
  SStateRef s ref -> -- ^ Singleton representing the state reference.
  Proxy field -> -- ^ Proxy for the field name to corrupt.
  GYTxSkeleton v -> -- ^ The current skeleton.
  SAppInstance app -> -- ^ Application instance context.
  GYNetworkId ->
  GYProviders ->
  Data -> -- ^ The corrupting Plutus 'Data'.
  TxBuilder v (Either Text (GYTxSkeleton v)) -- ^ Resulting skeleton or error.
corruptFieldInStateRef stateRef fieldProxy skeleton appInstance networkId providers corruptionValue = do
  let fieldName = symbolVal fieldProxy
  liftIO $ putStrLn $ "🎯 Corrupting field '" ++ fieldName ++ "' in state type: " ++ unpack (extractStateFromRef stateRef)

  -- Find the output(s) in the current skeleton that match the state reference.
  matchingOutputs <- findOutputsMatchingStateRef appInstance stateRef (gytxOuts skeleton) networkId providers

  case matchingOutputs of
    [] -> return $ Left "No matching outputs found for the state reference to corrupt."
    (outputIndex, originalOutput):_ -> do -- Corrupt only the first match found.
      -- Corrupt the field in the identified output.
      corruptedOutput <- liftIO $ corruptFieldInOutputWithStateType
        originalOutput
        fieldProxy
        (getStateTypeFromRef stateRef)
        corruptionValue

      -- Replace the original output with the corrupted one in the skeleton.
      let newOutputs = replaceOutputAt outputIndex corruptedOutput (gytxOuts skeleton)
      let corruptedSkeleton = skeleton { gytxOuts = newOutputs }

      return $ Right corruptedSkeleton

-- | (Internal) Extracts the 'SStateSpec' singleton from an 'SStateRef'.
getStateTypeFromRef :: SStateRef s ref -> SStateSpec s
getStateTypeFromRef (STypedTheOnlyInstance spec) = spec
getStateTypeFromRef (STypedUniqueWhere spec _) = spec
getStateTypeFromRef (STypedAny spec) = spec
getStateTypeFromRef (STypedAnyWhere spec _) = spec
getStateTypeFromRef (STypedByLabel spec _) = spec

-- | (Internal) Finds outputs in a list that correspond to a specific resolved 'SStateRef'.
-- Resolves the reference to its expected UTxO and finds outputs matching its address and value.
findOutputsMatchingStateRef ::
  forall app s (ref :: TypedStateRef s) v pv.
  ( AppSpec app
  , AppValidatorScripts app
  , StateSpec s
  , StateDatumConstraints s
  , Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
  , SingPlutusVersionI pv
  ) =>
  SAppInstance app ->
  SStateRef s ref ->
  [GYTxOut v] ->
  GYNetworkId ->
  GYProviders ->
  TxBuilder pv [(Int, GYTxOut v)] -- Returns list of (index, output) pairs.
findOutputsMatchingStateRef appInstance stateRef outputs networkId providers = do
  liftIO $ putStrLn "🔍 Finding outputs matching state reference..."
  -- Resolve the state reference to the UTxO it points to (in the *input*).
  resolveResult <- resolveStateRefSingleton appInstance stateRef STupleNil [] networkId providers

  case resolveResult of
    Left err -> do
      liftIO $ putStrLn $ "❌ Failed to resolve state reference: " ++ show err
      return []
    Right (expectedUtxoRef, expectedUtxo) -> do
      liftIO $ putStrLn $ "✅ State reference resolves to UTxO: " ++ show expectedUtxoRef
      -- Find outputs that match the address and value of the resolved *input* UTxO.
      let matchingIndices = findOutputsMatchingUtxo expectedUtxo outputs
      return $ map (\i -> (i, outputs !! i)) matchingIndices


-- | (Internal) Finds the indices of 'GYTxOut's in a list that match the address and value of a given 'GYUTxO'.
findOutputsMatchingUtxo :: GYUTxO -> [GYTxOut v] -> [Int]
findOutputsMatchingUtxo expectedUtxo outputs =
  let indexedOutputs = zip [0..] outputs
      expectedAddress = utxoAddress expectedUtxo
      expectedValue = utxoValue expectedUtxo
  -- Filter outputs based on matching address and value.
  in map fst $ filter (matchesUtxo expectedAddress expectedValue . snd) indexedOutputs

-- | (Internal) Checks if a 'GYTxOut' has the same address and value as the expected ones.
matchesUtxo :: GYAddress -> GYValue -> GYTxOut v -> Bool
matchesUtxo expectedAddr expectedValue output =
  gyTxOutAddress output == expectedAddr &&
  gyTxOutValue output == expectedValue

-- | (Internal) Replaces an element at a specific index in a list. Returns the original list if index is out of bounds.
replaceOutputAt :: Int -> GYTxOut v -> [GYTxOut v] -> [GYTxOut v]
replaceOutputAt index newOutput outputs =
  let (before, after) = splitAt index outputs
  in case after of
    (_:rest) -> before ++ [newOutput] ++ rest -- Replace element
    [] -> outputs  -- Index out of bounds

-- | (Internal) Corrupts a specified field within the datum of a single 'GYTxOut'.
-- Uses 'corruptFieldUsingStateType' to perform the generic corruption based on the state type.
corruptFieldInOutputWithStateType ::
  forall s field v.
  ( StateSpec s
  , Generic (StateDatum s)
  , GCorruptField (Rep (StateDatum s))
  , GExtractField (Rep (StateDatum s))
  , StateDatumConstraints s
  , KnownSymbol field
  ) =>
  GYTxOut v -> -- ^ The output to corrupt.
  Proxy field -> -- ^ Proxy for the field name.
  SStateSpec s -> -- ^ Singleton representing the state type of the datum.
  Data -> -- ^ The corrupting Plutus 'Data'.
  IO (GYTxOut v) -- ^ The output with the corrupted datum.
corruptFieldInOutputWithStateType output fieldProxy stateType corruptionValue = do
  let fieldName = symbolVal fieldProxy
  putStrLn $ "🎯 Corrupting field '" ++ fieldName ++ "' using state type: " ++ show stateType

  case gyTxOutDatum output of
    Nothing -> do
      putStrLn "⚠️  No datum to corrupt"
      return output
    Just (originalDatum, datumType) -> do
      putStrLn $ "📝 Original datum: " ++ show originalDatum
      -- Perform the corruption on the GYDatum.
      corruptedDatum <- corruptFieldUsingStateType originalDatum fieldName stateType corruptionValue

      putStrLn $ "🔥 Corrupted datum: " ++ show corruptedDatum
      -- Return the output with the modified datum.
      return $ output { gyTxOutDatum = Just (corruptedDatum, datumType) }


-- | (Internal) The core generic datum corruption logic.
-- Decodes the 'GYDatum', uses 'corruptFieldGenerically' to modify the field, and re-encodes it.
corruptFieldUsingStateType ::
  forall s.
  ( StateSpec s
  , Generic (StateDatum s)
  , GCorruptField (Rep (StateDatum s))
  , GExtractField (Rep (StateDatum s))
  , StateDatumConstraints s
  ) =>
  GYDatum -> -- ^ The original datum.
  String -> -- ^ Field name to corrupt.
  SStateSpec s -> -- ^ Singleton representing the datum's type.
  Data -> -- ^ Corrupting Plutus 'Data'.
  IO GYDatum -- ^ The corrupted 'GYDatum'.
corruptFieldUsingStateType gyDatum fieldName (_stateType :: SStateSpec s) corruptionValue = do
  putStrLn "🔧 Converting datum to state data for corruption..."

  let builtinData = toBuiltinData gyDatum
  -- Decode the datum to the Haskell type.
  case fromBuiltinData builtinData :: Maybe (StateDatum s) of
    Nothing -> do
      putStrLn "❌ Failed to parse datum as state data"
      return gyDatum
    Just stateData -> do
      putStrLn "✅ Successfully parsed state data"
      -- Check if the field actually exists.
      case extractFieldFromDatum @s (pack fieldName) stateData of
        Nothing -> do
          putStrLn $ "⚠️  Field '" ++ fieldName ++ "' not found in state - returning original"
          return gyDatum
        Just (SomeFieldValue _originalValue) -> do
          putStrLn $ "✅ Found field '" ++ fieldName ++ "'"

          -- Use the generic corruption function from Modsefa.Test.GenericCorruption.
          corruptedResult <- corruptFieldInStateDataTyped @s stateData fieldName corruptionValue
          case corruptedResult of
            Left err -> do
              putStrLn $ "❌ Corruption failed: " ++ show err
              return gyDatum
            Right corruptedStateData -> do
              -- Re-encode the corrupted Haskell value back to GYDatum.
              let corruptedBuiltinData = toBuiltinData corruptedStateData
              return $ datumFromPlutus' corruptedBuiltinData

-- | (Internal) Typed wrapper around 'corruptFieldGenerically'.
corruptFieldInStateDataTyped ::
  forall s.
  ( StateSpec s
  , Generic (StateDatum s)
  , GCorruptField (Rep (StateDatum s))
  ) =>
  StateDatum s ->
  String ->
  Data ->
  IO (Either Text (StateDatum s))
corruptFieldInStateDataTyped stateData fieldName corruptionValue = do
  putStrLn $ "🎯 Generically corrupting field '" ++ fieldName ++ "' in state data"
  putStrLn "🔧 Using Generic machinery to find and corrupt field"

  -- Call the function from Modsefa.Test.GenericCorruption.
  case corruptFieldGenerically @s stateData fieldName corruptionValue of
    Left err -> do
      putStrLn $ "❌ Generic corruption failed: " ++ show err
      return $ Left err
    Right corruptedData -> do
      putStrLn "✅ Successfully corrupted using Generic infrastructure"
      return $ Right corruptedData

-- | (Internal) Finds the validator responsible for minting a specific token via 'OwnPolicy'.
findMintingValidatorForToken :: forall app action. AppValidatorScripts app
  => SAppSpec app
  -> SActionSpec action
  -> GYTokenName
  -> Maybe (SomeValidatorWithScript app)
findMintingValidatorForToken appSpec (SActionSpec _ steps _ _) targetTokenName = go steps
  where
    -- Recursively check the steps list.
    go :: SActionStepList ss -> Maybe (SomeValidatorWithScript app)
    go ASSLNil = Nothing
    go (ASSLCons step rest) =
      checkStep step <|> go rest

    -- Check if the step involves creating a tokenized state with OwnPolicy.
    checkStep :: SActionStep s -> Maybe (SomeValidatorWithScript app)
    checkStep (SOp (SCreate stateType _ _)) = checkStateType stateType
    checkStep (SMap (SCreate stateType _ _) _ _) = checkStateType stateType
    checkStep _ = Nothing

    -- Check if the state type uses OwnPolicy and matches the target token name.
    checkStateType :: forall s. StateSpec s => SStateSpec s -> Maybe (SomeValidatorWithScript app)
    checkStateType stype = case stype of
      SStateSpec _ idSing _ _ ->
        case getOwnPolicyTokenDetails idSing of
          Just (tn, _) | tn == targetTokenName ->
            -- If it matches, find the validator managing this state.
            case findValidatorManagingState appSpec (SomeStateType stype) of
              Right (SomeValidator v) -> findValidatorWithScript (getValidatorNameFromSingleton v) appSpec
              Left _ -> Nothing
          _ -> Nothing

-- | (Internal) Corrupts a single entry in the minting map ('gytxMint') of the skeleton.
-- Finds the entry matching the policy ID and original token name, then replaces the name.
corruptMintEntry ::
  CurrencySymbol -> -- Target policy ID.
  GYTokenName -> -- Original token name.
  GYTokenName -> -- Corrupted token name.
  (GYBuildScript v, (Data.Map.Map GYTokenName Integer, GYRedeemer)) -> -- Original mint entry.
  Either Text (GYBuildScript v, (Data.Map.Map GYTokenName Integer, GYRedeemer)) -- Corrupted entry or error.
corruptMintEntry targetPolicyId originalName corruptedName (mintScript, (tokenMap, redeemer)) =
  -- Check if the script's policy ID matches the target.
  if buildScriptToCurrencySymbol mintScript == targetPolicyId
  then
    -- Check if the original token name exists in the map for this policy.
    case Data.Map.lookup originalName tokenMap of
      Nothing -> Left $ "Original token name not found in minting map: " <> pack (show originalName)
      Just quantity ->
        -- Replace the token name: Insert corrupted, Delete original.
        let newMap = Data.Map.insert corruptedName quantity (Data.Map.delete originalName tokenMap)
        in Right (mintScript, (newMap, redeemer))
  else
    -- Not the target policy ID, return the entry unchanged.
    Right (mintScript, (tokenMap, redeemer))

-- | (Internal) Extracts the 'CurrencySymbol' from a 'GYBuildScript'.
buildScriptToCurrencySymbol :: GYBuildScript v -> CurrencySymbol
buildScriptToCurrencySymbol = mintingPolicyIdToCurrencySymbol . mintingPolicyIdFromWitness

-- | (Internal) Corrupts the token name within the 'GYValue' of a 'GYTxOut'.
-- Finds assets matching the policy ID and original token name, then replaces the name.
corruptOutputTokenName :: GYMintingPolicyId -> GYTokenName -> GYTokenName -> GYTxOut v -> GYTxOut v
corruptOutputTokenName targetPolicyId originalTokenName corruptedTokenName txOut =
  let
    originalValue = gyTxOutValue txOut
    (ada, nonAdaValue) = valueSplitAda originalValue

    -- Convert the non-ADA value to a list for easier processing.
    nonAdaList = valueToList nonAdaValue

    -- Map over the asset list, replacing the token name if it matches.
    newAssetList = map (\(asset, qty) ->
      case asset of
        GYToken policyId tokenName | policyId == targetPolicyId && tokenName == originalTokenName ->
          -- Found a match, replace the token name.
          (GYToken policyId corruptedTokenName, qty)
        _ -> (asset, qty)
      ) nonAdaList

    -- Reconstruct the value from the ADA part and the modified asset list.
    finalValue = valueFromLovelace ada <> valueFromList newAssetList
  in
    -- Return the output with the corrupted value.
    txOut { gyTxOutValue = finalValue }

-- | (Internal) Checks if a 'GYTxOut' contains a datum that decodes successfully to the specified state type 's'.
isOutputOfStateType' :: forall s v. (StateSpec s, StateDatumConstraints s) => SStateSpec s -> GYTxOut v -> Bool
isOutputOfStateType' _ output =
  case gyTxOutDatum output of
    Nothing -> False
    Just (datum, _) ->
      let bd = toBuiltinData datum
      in case fromBuiltinData bd :: Maybe (StateDatum s) of
        Just _ -> True
        Nothing -> False

-- | (Internal) Finds the 'GYTxOutRef' in a set that corresponds to a UTXO containing a datum of the specified state type.
findOriginalRefInput :: forall s. (StateSpec s, StateDatumConstraints s)
                     => Data.Set.Set GYTxOutRef -- ^ The set of reference input Refs.
                     -> SStateSpec s            -- ^ The state spec to look for.
                     -> GYNetworkId
                     -> GYProviders
                     -> IO (Either Text GYTxOutRef)
findOriginalRefInput refs spec networkId providers = do
  -- Check each reference input UTxO provided in the set.
  results <- mapM findDatumForRef (Data.Set.toList refs)
  case catMaybes results of
    [foundRef] -> return $ Right foundRef
    [] -> return $ Left $ "Could not find any reference input matching state type " <> getStateName spec
    _  -> return $ Left $ "Found multiple reference inputs matching state type " <> getStateName spec
  where
    -- Helper to check a single TxOutRef.
    findDatumForRef :: GYTxOutRef -> IO (Maybe GYTxOutRef)
    findDatumForRef ref = do
      eUtxo <- try $ runGYTxQueryMonadIO networkId providers $ utxoAtTxOutRef ref
      case eUtxo of
        Left (_ :: SomeException) -> return Nothing
        Right Nothing -> return Nothing
        Right (Just utxo) ->
          -- Check if the UTxO has an inline datum of the correct type.
          case utxoOutDatum utxo of
            GYOutDatumInline datum ->
              let bd = toBuiltinData datum
              in case fromBuiltinData bd :: Maybe (StateDatum s) of
                   Just _ -> return $ Just ref
                   Nothing -> return Nothing
            _ -> return Nothing