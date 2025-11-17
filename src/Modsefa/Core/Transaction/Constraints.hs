{-|
Module      : Modsefa.Core.Transaction.Constraints
Description : Processing Modsefa constraints during transaction building.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module handles the processing of 'Modsefa.Core.Foundation.Types.TypedConstraint's defined
within a 'Modsefa.Core.Foundation.Types.TypedActionSpec'. It translates these high-level
constraints into specific requirements for the transaction skeleton being built
('GeniusYield.TxBuilder.GYTxSkeleton'). This includes:

- Extracting required signers ('GYPubKeyHash') based on 'MustBeSignedByState' and 'MustBeSignedByParam'.
- Identifying required reference inputs ('GYTxOutRef') based on constraints like 'MustBeSignedByState'.
- Generating necessary outputs for aggregate state changes ('MustAddToAggregateState').
- Generating necessary inputs and outputs for aggregate state withdrawals ('MustWithdrawFromAggregateState').
- Filtering redundant constraints (e.g., if an operation already consumes a UTxO that a constraint would otherwise require as a reference input).
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Constraint processing for transaction building
--
-- This module handles processing of constraints in action specifications,
-- including extracting required signers and reference inputs from constraints
-- like MustBeSignedByState.
--
-- Organization:
--   1. Main Constraint Processing
--   2. Reference Input Processing
--   3. Signer Extraction
--   4. Output Processing
--   5. Constraint Filtering
--   6. Type Families & Validation
--   7. Utility Functions
module Modsefa.Core.Transaction.Constraints
  ( -- * Main Constraint Processing
    processConstraintsDirectly

    -- * Type Families & Validation
  , ActionConstraintsValid
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Kind (Constraint)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text, pack)
import Data.Typeable (Typeable, cast)
import GHC.TypeLits (symbolVal)

import GeniusYield.TxBuilder
  ( GYTxQueryMonad(utxosAtAddress), GYTxSkeleton(..), mustBeSignedBy, mustHaveInput
  , mustHaveOutput, mustHaveRefInput, runGYTxQueryMonadIO
  )
import GeniusYield.Types
  ( GYBuildPlutusScript(GYBuildPlutusScriptInlined), GYNetworkId, GYOutDatum(..)
  , GYProviders, GYPubKeyHash, GYTxIn(GYTxIn), GYTxInWitness(GYTxInWitnessScript)
  , GYTxOutRef, GYUTxO(..), GYValue, PlutusVersion(PlutusV3)
  , SingPlutusVersion(SingPlutusV3), SingPlutusVersionI (singPlutusVersion)
  , addressFromPlutus, datumFromPlutus', isEmptyValue, mkGYTxOut
  , pubKeyHashFromPlutus, redeemerFromPlutusData, scriptVersion, unitDatum
  , utxosToList, valueMinus, valueToPlutus
  )
import PlutusLedgerApi.V1.Value (geq)
import PlutusLedgerApi.V3 (BuiltinData(BuiltinData), PubKeyHash, toData)
import PlutusTx (fromBuiltinData, toBuiltinData)

import Modsefa.Core.Foundation
  ( AppSpec(AppInstanceParameters, Validators), ExtractStateFromConstraint
  , MaybeStateDatumConstraints, ParamsToValue, ResolveInstanceParamList
  , SomeFieldValue(SomeFieldValue), StateDatum, TypedConstraint
  )
import Modsefa.Core.Singletons
  ( SAppInstance(..), SAppSpec, SConstraint(..), SConstraintList(..)
  , SInstanceParams, SOperation(SUpdate), SOperationList(..), SParamTuple(STupleNil)
  , SStateRef, SValidator, SomeStateType(SomeStateType)
  , SomeValidator(SomeValidator), extractFieldFromDatum
  , extractParamByNameDirectWithProxy, extractStateFromRef
  , findValidatorManagingState, getStateName, getValidatorNameFromSingleton
  )
import Modsefa.Core.ValidatorScript (AppValidatorScripts(getValidatorScript))

import Modsefa.Core.Transaction.Context (TxBuilder)
import Modsefa.Core.Transaction.Operations
  ( resolveAddressForTx, resolveStateRefSingleton, resolveValueForTx
  )
import Modsefa.Core.Transaction.Types (RedeemerPolicy(UseNamedRedeemer))
import Modsefa.Core.Transaction.Utils
  ( extractValidatorParamsFromAppInstance, findParameterDerivationFor
  , getValidatorAddressFromInstance, resolveValidatorParameters
  , resolveValidatorParametersFromAllSources
  )


-- ============================================================================
-- 1. MAIN CONSTRAINT PROCESSING
-- ============================================================================

-- | Processes a list of 'SConstraint's for a given action, applying their effects
-- (adding signers, reference inputs, aggregate state outputs/inputs) to a transaction skeleton.
-- It first processes constraints that generate outputs ('MustAddToAggregateState', 'MustWithdrawFromAggregateState'),
-- then filters redundant constraints, then adds reference inputs, and finally adds required signers.
processConstraintsDirectly ::
  forall app constraints operations params pv.
  ( Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
  , AppValidatorScripts app
  , AppSpec app
  , ActionConstraintsValid constraints
  , SingPlutusVersionI pv
  ) =>
  SConstraintList constraints -> -- ^ Singleton list of constraints for the action.
  SOperationList operations -> -- ^ Singleton list of operations (used for filtering).
  SAppSpec app -> -- ^ Application specification singleton.
  SParamTuple params -> -- ^ Action parameters tuple.
  [Text] -> -- ^ Action parameter names list.
  SInstanceParams app -> -- ^ Instance parameters.
  RedeemerPolicy -> -- ^ Redeemer policy for the action.
  GYNetworkId -> -- ^ Network ID.
  GYProviders -> -- ^ Blockchain providers.
  GYTxSkeleton pv -> -- ^ The current transaction skeleton.
  TxBuilder pv (Either Text (GYTxSkeleton pv)) -- ^ The updated skeleton or an error.
processConstraintsDirectly constraints operations appSpec' params paramNames instanceParams' redeemerPolicy networkId providers skeleton = do
  let appInstance = SAppInstance appSpec' instanceParams'
  -- 1. Process constraints that modify outputs/inputs (Aggregate States) first.
  paymentSkeletonResult <- processConstraintListForOutputs constraints appInstance params paramNames redeemerPolicy networkId providers skeleton
  case paymentSkeletonResult of
    Left err -> return $ Left err
    Right paymentSkeleton -> do
      -- 3. Process remaining constraints to identify required reference inputs.
      refInputsResult <- processConstraintListForRefs constraints operations appInstance params paramNames networkId providers
      case refInputsResult of
        Left err -> return $ Left err
        Right refInputs -> do
          -- 4. Process constraints to identify required signers.
          signersResult <- extractRequiredSigners constraints appInstance params paramNames networkId providers
          case signersResult of
            Left err -> return $ Left err
            Right requiredSigners -> do
              -- 5. Add reference inputs and signers to the skeleton.
              let skeletonWithRefs = addReferenceInputsToSkeleton refInputs paymentSkeleton
              let finalSkeleton = addRequiredSignersToSkeleton requiredSigners skeletonWithRefs
              return $ Right finalSkeleton

-- ============================================================================
-- 2. REFERENCE INPUT PROCESSING
-- ============================================================================

-- | (Internal) Recursively processes an 'SConstraintList' to collect all required reference inputs ('GYTxOutRef').
processConstraintListForRefs ::
  forall constraints app operations params pv.
  ( AppValidatorScripts app
  , AppSpec app
  , Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
  , ActionConstraintsValid constraints
  , SingPlutusVersionI pv
  ) =>
  SConstraintList constraints ->
  SOperationList operations ->
  SAppInstance app ->
  SParamTuple params ->
  [Text] ->
  GYNetworkId ->
  GYProviders ->
  TxBuilder pv (Either Text [GYTxOutRef]) -- ^ List of required reference input UTxO Refs or error.
processConstraintListForRefs SCLNil _ _ _ _ _ _ = return $ Right [] -- Base case
processConstraintListForRefs (SCLCons constraint rest) operations appInstance params paramNames networkId providers = do
  -- Check if the constraint is redundant (this is the filtering logic)
  if isConstraintRedundantForOperations constraint operations
    -- If redundant, skip it and process the rest
    then processConstraintListForRefs rest operations appInstance params paramNames networkId providers
    -- If not redundant, process it as normal
    else do
      -- Process head constraint
      thisResult <- processSingleConstraintForRefs constraint appInstance params paramNames networkId providers
      -- Process tail constraints (pass 'operations' along)
      restResult <- processConstraintListForRefs rest operations appInstance params paramNames networkId providers
      -- Combine results or propagate error
      case (thisResult, restResult) of
        (Right refs1, Right refs2) -> return $ Right (refs1 ++ refs2)
        (Left err, _) -> return $ Left err
        (_, Left err) -> return $ Left err

-- | (Internal) Processes a single 'SConstraint' to determine if it requires a reference input.
processSingleConstraintForRefs ::
  forall constraint app params pv.
  ( AppValidatorScripts app
  , AppSpec app
  , Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
  , MaybeStateDatumConstraints (ExtractStateFromConstraint constraint)
  , SingPlutusVersionI pv
  ) =>
  SConstraint constraint ->
  SAppInstance app ->
  SParamTuple params ->
  [Text] ->
  GYNetworkId ->
  GYProviders ->
  TxBuilder pv (Either Text [GYTxOutRef]) -- ^ List containing the required ref input (if any), or error.
processSingleConstraintForRefs constraint appInstance params paramNames networkId providers = case constraint of
  -- MustBeSignedByState requires the state UTxO as a reference input to read the PKH field.
  SMustBeSignedByState (stateRef :: SStateRef s ref) _fieldProxy -> do
    -- Resolve the state reference to the actual UTxO Ref.
    result <- resolveStateRefSingleton @s appInstance stateRef params paramNames networkId providers
    case result of
      Left err -> return $ Left err
      Right (utxoRef', _utxo) -> return $ Right [utxoRef']
  _ -> return $ Right []

-- ============================================================================
-- 3. SIGNER EXTRACTION
-- ============================================================================

-- | (Internal) Recursively processes an 'SConstraintList' to collect all required signer public key hashes ('GYPubKeyHash').
extractRequiredSigners ::
  forall constraints app params pv.
  ( AppValidatorScripts app
  , AppSpec app
  , Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
  , ActionConstraintsValid constraints
  , SingPlutusVersionI pv
  ) =>
  SConstraintList constraints ->
  SAppInstance app ->
  SParamTuple params ->
  [Text] ->
  GYNetworkId ->
  GYProviders ->
  TxBuilder pv (Either Text [GYPubKeyHash]) -- ^ List of required signer PKHs or error.
extractRequiredSigners SCLNil _ _ _ _ _ = return $ Right [] -- Base case
extractRequiredSigners (SCLCons constraint rest) appInstance params paramNames networkId providers = do
  -- Process head constraint
  thisResult <- extractSignerFromConstraint constraint appInstance params paramNames networkId providers
  -- Process tail constraints
  restResult <- extractRequiredSigners rest appInstance params paramNames networkId providers
  -- Combine results or propagate error
  case (thisResult, restResult) of
    (Right signers1, Right signers2) -> return $ Right (signers1 ++ signers2)
    (Left err, _) -> return $ Left err
    (_, Left err) -> return $ Left err

-- | Extracts the required signer ('GYPubKeyHash', if any) from a single 'SConstraint'.
extractSignerFromConstraint ::
  forall constraint app params pv.
  ( AppValidatorScripts app
  , AppSpec app
  , Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
  , MaybeStateDatumConstraints (ExtractStateFromConstraint constraint)
  , SingPlutusVersionI pv
  ) =>
  SConstraint constraint ->
  SAppInstance app ->
  SParamTuple params ->
  [Text] ->
  GYNetworkId ->
  GYProviders ->
  TxBuilder pv (Either Text [GYPubKeyHash]) -- ^ List containing the signer PKH (if required), or error.
extractSignerFromConstraint constraint appInstance params paramNames networkId providers = case constraint of
  -- MustBeSignedByState: Resolve the state, extract the PKH field, convert to GYPubKeyHash.
  SMustBeSignedByState (stateRef :: SStateRef s ref) (fieldProxy :: Proxy field) -> do
    -- Resolve the state reference to find the UTxO containing the PKH.
    result <- resolveStateRefSingleton @s appInstance stateRef params paramNames networkId providers
    case result of
      Left err -> return $ Left err
      Right (_utxoRef, utxo) -> do
        let fieldName = pack $ symbolVal (fieldProxy :: Proxy field)
        -- Extract the datum and parse it.
        case utxoOutDatum utxo of
          GYOutDatumInline gyDatum -> do
            let builtinData = toBuiltinData gyDatum
            -- Ensure type application matches the state reference's type 'st'.
            case fromBuiltinData builtinData :: Maybe (StateDatum s) of
              Just (stateData :: StateDatum s) -> do
                -- Extract the specific field value.
                case extractFieldFromDatum @s fieldName stateData of
                  Just (SomeFieldValue fieldValue) -> do
                    -- Cast the field value to Plutus PubKeyHash.
                    case cast fieldValue of
                      Just (plutusPkh :: PubKeyHash) -> do
                        -- Convert Plutus PKH to Genius Yield PKH.
                        case pubKeyHashFromPlutus plutusPkh of
                          Left err -> return $ Left $ "Failed to convert PubKeyHash: " <> pack (show err)
                          Right gyPkh -> return $ Right [gyPkh]
                      Nothing -> return $ Left $ "Field " <> fieldName <> " is not a PubKeyHash"
                  Nothing -> return $ Left $ "Field " <> fieldName <> " not found in state"
              Nothing -> return $ Left "Failed to parse state data"
          _ -> return $ Left "UTxO does not have inline datum"
  SMustBeSignedByParam paramProxy -> do
    let paramName = pack $ symbolVal paramProxy
    -- Extract the parameter value, expecting PubKeyHash type.
    case extractParamByNameDirectWithProxy (Proxy @PubKeyHash) paramName paramNames params of
      Left err -> return $ Left $ "Could not extract PubKeyHash from action parameter '" <> paramName <> "': " <> err
      Right pkh ->
        -- Convert Plutus PKH to Genius Yield PKH.
        case pubKeyHashFromPlutus pkh of
          Left err' -> return $ Left $ "Failed to convert PubKeyHash from parameter '" <> paramName <> "': " <> pack (show err')
          Right gyPkh -> return $ Right [gyPkh]
  _ -> return $ Right []


-- ============================================================================
-- 4. OUTPUT PROCESSING
-- ============================================================================

-- | (Internal) Processes a single constraint to potentially add outputs or inputs to the skeleton,
-- specifically handling 'SMustAddToAggregateState' and 'SMustWithdrawFromAggregateState'.
processSingleConstraintForOutput ::
  forall constraint app params pv.
  ( AppValidatorScripts app
  , AppSpec app
  , Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
  , SingPlutusVersionI pv
  ) =>
  SConstraint constraint ->
  SAppInstance app ->
  SParamTuple params ->
  [Text] ->
  RedeemerPolicy ->
  GYNetworkId ->
  GYProviders ->
  GYTxSkeleton pv ->
  TxBuilder pv (Either Text (GYTxSkeleton pv))
processSingleConstraintForOutput constraint appInstance params paramNames redeemerPolicy networkId providers skeleton = case constraint of
  -- MustAddToAggregateState: Find managing validator, get its address, resolve value, add output.
  SMustAddToAggregateState stateTypeProxy valueProxy -> do
    -- Find the validator managing this aggregate state.
    case findValidatorManagingState (appSpec appInstance) (SomeStateType stateTypeProxy) of
      Left err -> return $ Left $ "Failed to find validator for aggregate state '" <> getStateName stateTypeProxy <> "': " <> err
      Right (SomeValidator validator) -> do
        let validatorName = getValidatorNameFromSingleton validator
        -- Get the validator's on-chain address.
        eAddress <- liftIO $ getValidatorAddressFromInstance validatorName appInstance networkId providers
        case eAddress of
          Left err -> return $ Left $ "Failed to get address for validator '" <> validatorName <> "' (MustAddToAggregateState): " <> err
          Right address -> do
            -- Resolve the value to be added from action/derived parameters.
            eValue <- resolveValueForTx valueProxy params paramNames appInstance networkId providers
            case eValue of
              Left err -> return $ Left $ "Failed to resolve value for MustAddToAggregateState ('" <> getStateName stateTypeProxy <> "'): " <> err
              Right value -> do
                -- Create the output (address, value, unit datum) and add it to the skeleton.
                -- Uses unitDatum as aggregate states don't typically carry specific datum.
                let output = mkGYTxOut address value unitDatum
                return $ Right $ skeleton <> mustHaveOutput output

  -- MustWithdrawFromAggregateState: Find validator, resolve value/address, find input UTXO, add input & outputs.
  SMustWithdrawFromAggregateState stateTypeProxy valueProxy addressProxy -> do
    -- Find the validator managing the aggregate state.
    case findValidatorManagingState (appSpec appInstance) (SomeStateType stateTypeProxy) of
      Left err -> return $ Left $ "Failed to find validator for aggregate state '" <> getStateName stateTypeProxy <> "' (MustWithdraw): " <> err
      Right (SomeValidator (validator :: SValidator v)) -> do
        let validatorName = getValidatorNameFromSingleton validator
        -- Get the treasury validator's address.
        eTreasuryAddress <- liftIO $ getValidatorAddressFromInstance validatorName appInstance networkId providers
        case eTreasuryAddress of
          Left err -> return $ Left $ "Failed to get address for treasury validator '" <> validatorName <> "' (MustWithdraw): " <> err
          Right treasuryAddress -> do
            -- Resolve the withdrawal value.
            eWithdrawalValue <- resolveValueForTx valueProxy params paramNames appInstance networkId providers
            case eWithdrawalValue of
              Left err -> return $ Left $ "Failed to resolve withdrawal value (MustWithdraw): " <> err
              Right withdrawalValue -> do
                -- 4. Resolve the destination address.
                case resolveAddressForTx addressProxy params paramNames of
                  Left err -> return $ Left $ "Failed to resolve destination address: " <> err
                  Right destinationAddress -> do
                    -- 5. Find a suitable UTXO at the treasury address to spend.
                    eTreasuryUtxos <- liftIO $ try $ runGYTxQueryMonadIO networkId providers $ utxosAtAddress treasuryAddress Nothing
                    case eTreasuryUtxos of
                      Left (e :: SomeException) -> return $ Left $ "Failed to query treasury UTXOs: " <> pack (show e)
                      Right treasuryUtxos -> do
                        let suitableUtxos = filter (\u -> utxoValue u `valueGreaterOrEqual` withdrawalValue) (utxosToList treasuryUtxos)
                        case suitableUtxos of
                          [] -> return $ Left "No single UTXO in treasury has sufficient funds for withdrawal."
                          (utxoToSpend:_) -> do -- Just pick the first suitable one for simplicity
                            -- 6. Construct the withdrawal transaction components.
                            let treasuryValue = utxoValue utxoToSpend
                                remainingValue = treasuryValue `valueMinus` withdrawalValue

                            -- We need to get the script for the witness
                            eValidatorParams <- liftIO $ case findParameterDerivationFor validatorName (appSpec appInstance) of
                                Nothing ->
                                    -- Fallback for non-derived parameters
                                    return $ extractValidatorParamsFromAppInstance validator (instanceParams appInstance)
                                Just derivation -> do
                                    -- Path for derived parameters
                                    derivedParamsResult <- resolveValidatorParameters validatorName derivation appInstance networkId providers
                                    case derivedParamsResult of
                                        Left err -> return $ Left err
                                        Right derivedParams ->
                                            return $ resolveValidatorParametersFromAllSources validator appInstance (Just derivedParams) STupleNil []

                            case eValidatorParams of
                              Left err -> return $ Left err
                              Right validatorParams -> do
                                let script = getValidatorScript @app (Proxy @v) validatorParams
                                    redeemer = case redeemerPolicy of
                                                  UseNamedRedeemer name -> redeemerFromPlutusData name
                                                  _                   -> redeemerFromPlutusData () 
                                    spv = singPlutusVersion @pv
                                    
                                case (spv, scriptVersion script) of
                                  (SingPlutusV3, SingPlutusV3) -> do
                                    let buildPlutusScript :: GYBuildPlutusScript 'PlutusV3 = GYBuildPlutusScriptInlined script
                                    let inputDatum = datumFromPlutus' (BuiltinData (toData ()))
                                    let spendInput = mustHaveInput $ GYTxIn (utxoRef utxoToSpend) (GYTxInWitnessScript
                                          buildPlutusScript
                                          (Just inputDatum)
                                          redeemer)

                                    -- Handle the Either result from addressFromPlutus
                                    case addressFromPlutus networkId destinationAddress of
                                      Left err -> return $ Left $ "Failed to convert destination address: " <> pack (show err)
                                      Right gyDestinationAddress -> do
                                        let withdrawalOutput = mustHaveOutput $ mkGYTxOut gyDestinationAddress withdrawalValue (datumFromPlutus' (BuiltinData (toData ())))
                                        let changeOutput = if isEmptyValue remainingValue
                                                           then mempty -- Don't create a 0 ADA output!
                                                           else mustHaveOutput $ mkGYTxOut treasuryAddress remainingValue (datumFromPlutus' (BuiltinData (toData ())))

                                        return $ Right $ skeleton <> spendInput <> withdrawalOutput <> changeOutput

                                  -- Handle version mismatch
                                  _ -> return $ Left "Plutus version mismatch between transaction and validator script for withdrawal!"
  _ -> return $ Right skeleton

-- | (Internal) Recursively processes an 'SConstraintList' by calling 'processSingleConstraintForOutput'.
processConstraintListForOutputs ::
  forall constraints app params pv.
  ( AppValidatorScripts app
  , AppSpec app
  , Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
  , SingPlutusVersionI pv
  ) =>
  SConstraintList constraints ->
  SAppInstance app ->
  SParamTuple params ->
  [Text] ->
  RedeemerPolicy ->
  GYNetworkId ->
  GYProviders ->
  GYTxSkeleton pv ->
  TxBuilder pv (Either Text (GYTxSkeleton pv))
processConstraintListForOutputs SCLNil _ _ _ _ _ _ skeleton = return $ Right skeleton -- Base case
processConstraintListForOutputs (SCLCons constraint rest) appInstance params paramNames redeemerPolicy networkId providers skeleton = do
  -- Process head constraint
  skeletonResult <- processSingleConstraintForOutput constraint appInstance params paramNames redeemerPolicy networkId providers skeleton
  -- On success, process tail constraints with the updated skeleton
  case skeletonResult of
    Left err -> return $ Left err
    Right newSkeleton -> processConstraintListForOutputs rest appInstance params paramNames redeemerPolicy networkId providers newSkeleton


-- ============================================================================
-- 5. CONSTRAINT FILTERING
-- ============================================================================

-- | (Internal) Checks if a single 'SConstraint' is made redundant by *any* operation in the 'SOperationList'.
isConstraintRedundantForOperations :: SConstraint constraint -> SOperationList operations -> Bool
isConstraintRedundantForOperations _constraint SOLNil = False -- Base case: No operations, cannot be redundant.
isConstraintRedundantForOperations constraint (SOLCons operation rest) =
  -- Check if redundant w.r.t current operation OR any subsequent operation.
  isConstraintRedundantForOperation constraint operation ||
  isConstraintRedundantForOperations constraint rest

-- | (Internal) Checks if a single 'SConstraint' is made redundant by a single 'SOperation'.
-- Currently, only considers 'SMustBeSignedByState' redundant if an 'SUpdate' operation
-- acts on the *exact same state reference*. This assumes the 'SUpdate' implicitly requires
-- the state as an input, making the explicit reference input from the constraint unnecessary.
isConstraintRedundantForOperation :: SConstraint constraint -> SOperation op -> Bool
isConstraintRedundantForOperation constraint operation = case (constraint, operation) of
  -- If constraint requires signing by state X, and operation updates state X...
  (SMustBeSignedByState constraintStateRef _, SUpdate operationStateRef _ _) ->
    -- ...check if they refer to the same state instance.
    sameStateRef constraintStateRef operationStateRef
  _ -> False
  where
    -- Helper to check if two SStateRefs likely refer to the same state instance
    -- based on their structure and name.
    sameStateRef :: SStateRef st1 ref1 -> SStateRef st2 ref2 -> Bool
    sameStateRef ref1 ref2 =
      extractStateFromRef ref1 == extractStateFromRef ref2

-- ============================================================================
-- 6. TYPE FAMILIES & VALIDATION
-- ============================================================================

-- | Constraint: Ensures all 'TypedConstraint's in a list satisfy 'ConstraintHasRequiredInstances'.
-- Used for compile-time validation of action specifications.
type family ActionConstraintsValid (constraints :: [TypedConstraint]) :: Constraint where
  ActionConstraintsValid '[] = ()
  ActionConstraintsValid (constraint ': rest) =
    ( ConstraintHasRequiredInstances constraint
    , ActionConstraintsValid rest
    )

-- | Constraint: Ensures a single 'TypedConstraint' has the necessary type class instances
-- (via 'StateDatumConstraints') required for its processing, if it references a state.
type family ConstraintHasRequiredInstances (constraint :: TypedConstraint) :: Constraint where
  -- This now correctly uses our new machinery.
  ConstraintHasRequiredInstances constraint =
    MaybeStateDatumConstraints (ExtractStateFromConstraint constraint)

-- ============================================================================
-- 7. UTILITY FUNCTIONS
-- ============================================================================

-- | (Internal) Helper to check if value v1 is greater than or equal to v2.
valueGreaterOrEqual :: GYValue -> GYValue -> Bool
valueGreaterOrEqual v1 v2 = valueToPlutus v1 `geq` valueToPlutus v2

-- | Adds a list of required reference inputs ('GYTxOutRef') to a transaction skeleton.
addReferenceInputsToSkeleton :: [GYTxOutRef] -> GYTxSkeleton pv -> GYTxSkeleton pv
addReferenceInputsToSkeleton refInputs skeleton =
  -- Fold over the list, adding each ref input using 'mustHaveRefInput'.
  foldl (\skel refInput -> skel <> mustHaveRefInput refInput) skeleton refInputs

-- | Adds a list of required signers ('GYPubKeyHash') to a transaction skeleton.
addRequiredSignersToSkeleton :: [GYPubKeyHash] -> GYTxSkeleton pv -> GYTxSkeleton pv
addRequiredSignersToSkeleton signers skeleton =
  -- Fold over the list, adding each signer using 'mustBeSignedBy'.
  foldl (\skel signer -> skel <> mustBeSignedBy signer) skeleton signers