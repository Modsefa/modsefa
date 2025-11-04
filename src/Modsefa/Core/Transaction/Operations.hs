{-|
Module      : Modsefa.Core.Transaction.Operations
Description : Processing of individual action steps and operations during transaction building.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module implements the core logic for translating 'Modsefa.Core.Foundation.Types.ActionStep's
(like 'Op', 'Let', 'Map') and their underlying 'Modsefa.Core.Foundation.Types.TypedOperation's
('Create', 'Update', 'Delete', 'Reference') into components of a 'GeniusYield.TxBuilder.GYTxSkeleton'.
It uses a type class ('ProcessActionSteps') for recursive processing of step lists and handles
details like datum construction, input/output generation, minting/burning policies, and state resolution.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Operation processing for transaction building
module Modsefa.Core.Transaction.Operations
  ( -- * Operation List Processing
    processActionSteps

    -- * Helper Functions
  , resolveStateRefSingleton
  , resolveValueForTx
  , resolveAddressForTx

    -- * Type Classes
  , ProcessActionSteps
  ) where

import Control.Monad (filterM, foldM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (MonadState(..))
import Data.Kind (Constraint, Type)
import Data.Map (insert, toList)
import Data.Proxy (Proxy(Proxy))
import Data.Set (delete)
import Data.Text (Text, pack)
import Data.Typeable (Typeable, cast)
import GHC.Generics (Generic, Rep, from, to)
import GHC.TypeLits (KnownSymbol, Symbol, natVal, symbolVal)

import GeniusYield.Imports ((&))
import GeniusYield.TxBuilder
  ( GYTxQueryMonad(utxosAtAddress), GYTxSkeleton(..), GYTxSkeletonRefIns(..)
  , mustHaveInput, mustHaveOutput, mustHaveRefInput, mustMint, runGYTxQueryMonadIO
  , scriptAddress
  )
import GeniusYield.Types
  ( GYAddress, GYAssetClass(..), GYBuildPlutusScript(..), GYBuildScript(..)
  , GYNetworkId, GYOutDatum(..), GYProviders, GYTokenName, GYTxIn(..)
  , GYTxInWitness(..), GYTxOutRef, GYTxOutUseInlineDatum(..), GYUTxO(..), GYValue
  , PlutusVersion(PlutusV3), SingPlutusVersion(..), SingPlutusVersionI
  , addressFromScript, datumFromPlutus', gyTxOutDatumL, mkGYTxOut, mintingPolicyId
  , redeemerFromPlutusData, scriptVersion, singPlutusVersion, utxoOutDatum, utxoRef
  , utxoValue, utxosToList, valueFromLovelace, valueSingleton, valueToList
  )
import PlutusLedgerApi.V3 (Address, BuiltinData(..))
import PlutusPrelude ((.~))
import PlutusTx (fromBuiltinData, toBuiltinData, toData)

import Modsefa.Core.Foundation
  ( ActionSpecParameters, ActionSpecSteps, ActionStep(..), AllStateTypesGeneric
  , AppSpec(AppInstanceParameters, Validators), ExtractOpsFromActionSteps
  , ExtractStateType, ExtractStateTypes, GExtractField, GetStateData, ParamsToValue
  , PolicySource(ExternalPolicy, OwnPolicy), ResolveInstanceParamList
  , SStateType(..), SomeFieldValue(..), SomeStatedUTxO(SomeStatedUTxO)
  , StateIdentifier(AggregateAsset, TokenIdentified)
  , StateRepresentable(stateIdentifier), TypedActionSpec, TypedOperation
  , TypedPredicate
  )
import Modsefa.Core.Singletons
  ( AutoSingletonActionStep, AutoSingletonActionStepList, GBuildDatumFromSpecs(..)
  , SActionSpec(..), SActionStep(..), SActionStepList(..)
  , SAppInstance(SAppInstance), SAppSpec, SFieldSpec(SSetTo), SFieldSpecList(..)
  , SInstanceParams, SOperation(..), SParamTuple(STupleNil), SPredicate(..)
  , SStateRef(..), STypedValue(..), SValidator, SomeStateType(SomeStateType)
  , SomeValidator(SomeValidator), extractFieldFromReferencedUTxO
  , extractParamByNameDirectWithProxy, extractStateFromRef
  , findValidatorManagingState, getStateName, getValidatorNameFromSingleton
  , getValidatorParams
  )
import Modsefa.Core.ValidatorScript (AppValidatorScripts(..))

import Modsefa.Core.Transaction.Context
  ( DerivationContext, OperationResult(..),  SomeGYTxOut(..), TxBuilder
  , TxBuilderContext(..)
  )
import Modsefa.Core.Transaction.Parameters
  ( Mappable(toMappedParamTuple), buildParamsToValue
  )
import Modsefa.Core.Transaction.Predicate (evaluatePredicate)
import Modsefa.Core.Transaction.Types (RedeemerPolicy(..))
import Modsefa.Core.Transaction.Utils
  ( findParameterDerivationFor, resolveValidatorParameters
  )


-- ============================================================================
-- 1. TYPE FAMILIES & CONSTRAINTS
-- ============================================================================

-- | Extracts the element type from a list type.
type family ListItem (list :: Type) :: Type where
  ListItem [a] = a

-- | Looks up the type of a parameter by name within a type-level parameter list.
type family LookupParamType (name :: Symbol) (params :: [(Symbol, Type)]) :: Type where
  LookupParamType name ('(name, t) ': _) = t
  LookupParamType name (_ ': rest) = LookupParamType name rest
  -- Note: Implicitly fails with type error if name not found.

-- | Constraint: Ensures all 'StateType's involved in a list of 'TypedOperation's
-- have 'Generic' instances and support generic datum building ('GBuildDatumFromSpecs').
type family AllOperationsHaveGeneric (ops :: [TypedOperation]) :: Constraint where
  AllOperationsHaveGeneric '[] = ()
  AllOperationsHaveGeneric (op ': rest) =
    ( Generic (GetStateData (ExtractStateType op))
    , GBuildDatumFromSpecs (Rep (GetStateData (ExtractStateType op)))
    , AllOperationsHaveGeneric rest
    )

-- | Constraint: Ensures all 'StateType's involved in a list of 'TypedOperation's
-- have 'Generic' instances and support generic field extraction ('GExtractField'),
-- needed primarily for predicate evaluation.
type family AllOperationsHaveRequiredInstances (ops :: [TypedOperation]) :: Constraint where
  AllOperationsHaveRequiredInstances '[] = ()
  AllOperationsHaveRequiredInstances (op ': rest) =
    ( Generic (GetStateData (ExtractStateType op))
    , GExtractField (Rep (GetStateData (ExtractStateType op)))
    , AllOperationsHaveRequiredInstances rest
    )

-- ============================================================================
-- 2. OPERATION LIST PROCESSING
-- ============================================================================

-- | Type class defining the recursive processing logic for a list of 'ActionStep's.
-- Instances handle empty lists, 'Op', 'Let', and 'Map' steps.
class ( AllStateTypesGeneric (ExtractStateTypes (ExtractOpsFromActionSteps steps))
      , AppValidatorScripts app
      , SingPlutusVersionI pv
      , Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
      , AutoSingletonActionStepList steps
      ) => ProcessActionSteps app (action :: TypedActionSpec app) (steps :: [ActionStep]) (params :: [(Symbol, Type)]) pv where
  -- | The internal processing function run within the 'TxBuilder' monad.
  processActionSteps' ::
    SAppSpec app ->
    SActionSpec app action ->
    SActionStepList steps -> -- ^ Singleton for the steps being processed.
    SParamTuple params -> -- ^ Action parameters.
    [Text] -> -- ^ Action parameter names.
    SInstanceParams app -> -- ^ Instance parameters.
    RedeemerPolicy -> -- ^ Redeemer policy for the action.
    DerivationContext -> -- ^ Resolved derived parameters.
    GYNetworkId ->
    GYProviders ->
    GYTxSkeleton pv -> -- ^ The skeleton built so far.
    TxBuilder pv (Either Text (GYTxSkeleton pv)) -- ^ Updated skeleton or error.

-- | Base case: Processing an empty list of steps ('[]') returns the current skeleton unchanged.
instance ( AllStateTypesGeneric '[]
         , AppValidatorScripts app
         , SingPlutusVersionI pv
         , Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
         ) => ProcessActionSteps app action '[] params pv where
  processActionSteps' _ _ ASSLNil _ _ _ _ _ _ _ skeleton = return (Right skeleton)

-- | Recursive case: Processes the head step ('step') using 'ProcessSingleActionStep',
-- then recursively calls 'ProcessActionSteps' on the tail ('rest').
instance ( ProcessActionSteps app action rest params pv
         , ProcessSingleActionStep app action step params pv
         , AutoSingletonActionStep step
         , AllStateTypesGeneric (ExtractStateTypes (ExtractOpsFromActionSteps '[step]))
         , AllStateTypesGeneric (ExtractStateTypes (ExtractOpsFromActionSteps (step ': rest)))
         ) => ProcessActionSteps app action (step ': rest) params pv where
  processActionSteps' appSpec' actionSpec (ASSLCons step restSteps) allParams paramNames instanceParams' redeemerPolicy derivationContext networkId providers skeleton = do
    result <- processSingleActionStep' appSpec' actionSpec step allParams paramNames instanceParams' redeemerPolicy derivationContext networkId providers skeleton
    case result of
      Left err -> return (Left err)
      Right updatedSkeleton ->
        -- The recursive call is also now in the TxBuilder monad
        processActionSteps' appSpec' actionSpec restSteps allParams paramNames instanceParams' redeemerPolicy derivationContext networkId providers updatedSkeleton

-- | (Internal) Helper class to dispatch processing to the correct function based on the 'ActionStep' constructor ('Op', 'Let', 'Map').
-- This avoids needing overlapping instances directly in 'ProcessActionSteps'.
class ProcessSingleActionStep app (action :: TypedActionSpec app) (step :: ActionStep) (params :: [(Symbol, Type)]) pv where
  processSingleActionStep' ::
    AllStateTypesGeneric (ExtractStateTypes (ExtractOpsFromActionSteps '[step])) =>
    SAppSpec app ->
    SActionSpec app action ->
    SActionStep step ->
    SParamTuple params ->
    [Text] ->
    SInstanceParams app ->
    RedeemerPolicy ->
    DerivationContext ->
    GYNetworkId ->
    GYProviders ->
    GYTxSkeleton pv ->
    TxBuilder pv (Either Text (GYTxSkeleton pv))

-- | Instance for a normal 'Op' step.
instance ( AppValidatorScripts app, SingPlutusVersionI pv
         , Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
         , Generic (GetStateData (ExtractStateType op))
         , GExtractField (Rep (GetStateData (ExtractStateType op)))
         ) => ProcessSingleActionStep app action ('Op op) params pv where
  processSingleActionStep' = processSingleOp

-- | Instance for processing a 'Let' step. Delegates to 'processSingleLet'.
instance ( AppValidatorScripts app, SingPlutusVersionI pv
         , Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
         , Generic (GetStateData (ExtractStateType op))
         , GExtractField (Rep (GetStateData (ExtractStateType op)))
         , KnownSymbol label
         ) => ProcessSingleActionStep app action ('Let label op) params pv where
  processSingleActionStep' = processSingleLet

-- | Instance for processing a 'Map' step. Delegates to 'processSingleMap'.
instance ( AppValidatorScripts app, SingPlutusVersionI pv
         , Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
         , KnownSymbol param
         , Mappable (ListItem (LookupParamType param (ActionSpecParameters action)))
         , Generic (GetStateData (ExtractStateType op))
         , GExtractField (Rep (GetStateData (ExtractStateType op)))
         ) => ProcessSingleActionStep app action ('Map op param constraints) params pv where
  processSingleActionStep' = processSingleMap

-- | User-facing wrapper function that initiates the recursive processing of action steps.
-- Takes the application/action specs and parameters, and kicks off the 'ProcessActionSteps' type class resolution.
processActionSteps :: forall app action steps params pv.
                   ( steps ~ ActionSpecSteps action
                   , params ~ ActionSpecParameters action
                   , ProcessActionSteps app action steps params pv
                   ) =>
  SAppSpec app ->
  SActionSpec app action ->
  SParamTuple params ->
  [Text] ->
  SInstanceParams app ->
  RedeemerPolicy ->
  DerivationContext ->
  GYNetworkId ->
  GYProviders ->
  GYTxSkeleton pv ->
  TxBuilder pv (Either Text (GYTxSkeleton pv))
processActionSteps appSpec' actionSpec@(SActionSpec _ steps _ _) = processActionSteps' appSpec' actionSpec steps

-- | (Internal) Processes a single 'Op' step by calling 'processOperationDirectly'.
processSingleOp ::
  ( AllStateTypesGeneric (ExtractStateTypes '[op])
  , AppValidatorScripts app, SingPlutusVersionI pv
  , Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
  , Generic (GetStateData (ExtractStateType op))
  , GExtractField (Rep (GetStateData (ExtractStateType op)))
  ) =>
  SAppSpec app ->
  SActionSpec app action ->
  SActionStep ('Op op) ->
  SParamTuple params ->
  [Text] ->
  SInstanceParams app ->
  RedeemerPolicy ->
  DerivationContext ->
  GYNetworkId ->
  GYProviders ->
  GYTxSkeleton pv ->
  TxBuilder pv (Either Text (GYTxSkeleton pv))
processSingleOp appSpec' _ (SOp operation) params paramNames instanceParams' redeemerPolicy derivationContext networkId providers skeleton = do
  processOperationDirectly appSpec' operation params paramNames instanceParams' redeemerPolicy derivationContext networkId providers skeleton

-- | (Internal) Processes a single 'Let' step. It first processes the underlying operation,
-- then stores the result ('ORCreate' or 'ORReference') in the 'TxBuilderContext' ('tbcLetResults')
-- under the specified label.
processSingleLet ::
  forall app action label op params pv.
  ( AllStateTypesGeneric (ExtractStateTypes '[op])
  , AppValidatorScripts app, SingPlutusVersionI pv
  , Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
  , Generic (GetStateData (ExtractStateType op))
  , GExtractField (Rep (GetStateData (ExtractStateType op)))
  , KnownSymbol label
  ) =>
  SAppSpec app ->
  SActionSpec app action ->
  SActionStep ('Let label op) ->
  SParamTuple params ->
  [Text] ->
  SInstanceParams app ->
  RedeemerPolicy ->
  DerivationContext ->
  GYNetworkId ->
  GYProviders ->
  GYTxSkeleton pv ->
  TxBuilder pv (Either Text (GYTxSkeleton pv))
processSingleLet appSpec' _ (SLet labelProxy operation) params paramNames instanceParams' redeemerPolicy derivationContext networkId providers skeleton = do
  opResult <- processOperationDirectly appSpec' operation params paramNames instanceParams' redeemerPolicy derivationContext networkId providers skeleton
  case opResult of
    Left err -> return (Left err)
    Right newSkeleton -> do
      ctx <- get
      let mOpResult :: Maybe (OperationResult pv)
          mOpResult = case operation of
            -- For Create, find the newly added output. Assumes it's the last one.
            SCreate {} ->
              let newOutputs = gytxOuts newSkeleton
                  oldOutputs = gytxOuts skeleton
              in if length newOutputs > length oldOutputs
                  then Just $ ORCreate (SomeGYTxOut (last newOutputs))
                  else Nothing
            SReference stateRefProxy _ ->
              let refKey = pack $ show stateRefProxy
              in case lookup refKey (toList $ tbcResolvedRefs ctx) of
                  Just statedUtxo -> Just $ ORReference statedUtxo
                  Nothing         -> Nothing
            _ -> Nothing

      -- If a storable result was produced, add it to the context map.
      case mOpResult of
        Just opRes -> do
          let labelText = pack $ symbolVal labelProxy
          put $ ctx { tbcLetResults = insert labelText opRes (tbcLetResults ctx) }
        Nothing -> return ()

      return (Right newSkeleton)

-- | (Internal) Processes a 'Map' step. Extracts the list parameter, then iterates
-- through the list, processing the inner operation ('processOperationDirectly') for each item.
-- Uses 'foldM' to accumulate skeleton changes across iterations.
processSingleMap :: forall app action op param constraints params pv.
  ( AllStateTypesGeneric (ExtractStateTypes '[op])
  , AppValidatorScripts app, SingPlutusVersionI pv
  , Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
  , KnownSymbol param
  , Mappable (ListItem (LookupParamType param (ActionSpecParameters action)))
  , Generic (GetStateData (ExtractStateType op))
  , GExtractField (Rep (GetStateData (ExtractStateType op)))
  ) =>
  SAppSpec app ->
  SActionSpec app action ->
  SActionStep ('Map op param constraints) ->
  SParamTuple params ->
  [Text] ->
  SInstanceParams app ->
  RedeemerPolicy ->
  DerivationContext ->
  GYNetworkId ->
  GYProviders ->
  GYTxSkeleton pv ->
  TxBuilder pv (Either Text (GYTxSkeleton pv))
processSingleMap appSpec' _ (SMap innerOperation paramProxy _) allParams allParamNames instanceParams' redeemerPolicy derivationContext networkId providers skeleton = do
  let listParamName = pack $ symbolVal paramProxy
  case extractParamByNameDirectWithProxy (Proxy @[ListItem (LookupParamType param (ActionSpecParameters action))]) listParamName allParamNames allParams of
    Left err -> return $ Left ("Failed to extract list parameter '" <> listParamName <> "': " <> err)
    Right items -> do
      let innerParamNames = extractInnerParamNames innerOperation
      let folder skel item = do
            case skel of
              Left err -> return $ Left err
              Right currentSkeleton -> do
                -- Convert the list item into an SParamTuple using the Mappable instance.
                let itemAsParamTuple = toMappedParamTuple item
                processOperationDirectly appSpec' innerOperation itemAsParamTuple innerParamNames instanceParams' redeemerPolicy derivationContext networkId providers currentSkeleton
      foldM folder (Right skeleton) items

-- ============================================================================
-- 3. INDIVIDUAL OPERATION PROCESSING
-- ============================================================================

-- | Processes a single 'SOperation' ('Create', 'Update', 'Delete', 'Reference'),
-- modifying the 'GYTxSkeleton' accordingly. Handles datum building, finding validators,
-- resolving parameters, creating inputs/outputs, and minting/burning tokens.
processOperationDirectly ::
  forall app op params pv.
  ( AppValidatorScripts app
  , SingPlutusVersionI pv
  , Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
  , Generic (GetStateData (ExtractStateType op))
  , GExtractField (Rep (GetStateData (ExtractStateType op)))
  ) =>
  SAppSpec app ->
  SOperation op ->
  SParamTuple params -> -- ^ Action parameters (potentially subset for Map).
  [Text] -> -- ^ Names of parameters in the SParamTuple.
  SInstanceParams app -> -- ^ Instance parameters.
  RedeemerPolicy -> -- ^ Redeemer policy.
  DerivationContext -> -- ^ Resolved derived parameters.
  GYNetworkId ->
  GYProviders ->
  GYTxSkeleton pv -> -- ^ Skeleton to modify.
  TxBuilder pv (Either Text (GYTxSkeleton pv)) -- ^ Updated skeleton or error.
processOperationDirectly appSpec' operation params paramNames instanceParams' redeemerPolicy derivationContext networkId providers skeleton = case operation of

  SCreate stateType fieldSpecs _constraints -> do
    liftIO $ putStrLn $ "Processing Create for: " ++ show (getStateName stateType)
    datumResult <- buildDatumFromFieldSpecs stateType fieldSpecs params paramNames derivationContext networkId providers
    case datumResult of
      Left err -> return $ Left err
      Right datum -> do
        liftIO $ putStrLn $ "🎯 Final datum for " ++ show (getStateName stateType) ++ ": " ++ show datum
        case findValidatorManagingState appSpec' (SomeStateType stateType) of
          Left err -> return $ Left $ "No validator found for state: " <> err
          Right (SomeValidator (validator :: SValidator v)) -> do
            let validatorName = getValidatorNameFromSingleton validator
            liftIO $ putStrLn $ "📦 Found validator: " ++ show validatorName
            paramBuildResult <- case findParameterDerivationFor validatorName appSpec' of
              Nothing -> return $ buildParamsToValue (getValidatorParams validator) params paramNames instanceParams' Nothing
              Just derivation -> do
                derivedParamsResult <- liftIO $ resolveValidatorParameters validatorName derivation (SAppInstance appSpec' instanceParams') networkId providers
                case derivedParamsResult of
                  Left err -> return $ Left err
                  Right derivedParams -> return $ buildParamsToValue (getValidatorParams validator) params paramNames instanceParams' (Just derivedParams)

            case paramBuildResult of
              Left paramErr -> return $ Left $ "Failed to resolve validator parameters: " <> paramErr
              Right validatorParams -> do
                -- Get the validator script and calculate its address.
                let script = getValidatorScript @app (Proxy @v) validatorParams
                let spv = singPlutusVersion @pv
                scriptAddr <- liftIO $ runGYTxQueryMonadIO networkId providers $ scriptAddress script
                case (spv, scriptVersion script) of
                  (SingPlutusV3, SingPlutusV3) -> do
                    let (mintSkel, tokenValue) = case stateIdentifier stateType of
                          TokenIdentified OwnPolicy stTokenName tokenQuantity -> do
                            let policyId = mintingPolicyId script
                            let token = valueSingleton (GYToken policyId stTokenName) tokenQuantity
                            let buildPlutusScript :: GYBuildPlutusScript 'PlutusV3 = GYBuildPlutusScriptInlined script
                            let buildScript = GYBuildPlutusScript buildPlutusScript
                            let redeemer = case redeemerPolicy of
                                  UseNamedRedeemer name -> redeemerFromPlutusData name
                                  _ -> redeemerFromPlutusData ()
                            let mint = mustMint buildScript redeemer stTokenName tokenQuantity
                            (mint, token)
                          _ -> (mempty, mempty)
                    let outputValue = tokenValue
                    let gyDatum = datumFromPlutus' (BuiltinData (toData datum))
                    let baseOutput = mkGYTxOut scriptAddr outputValue gyDatum
                    let output = baseOutput & gyTxOutDatumL .~ GYTxOutUseInlineDatum
                    let outputSkel = mustHaveOutput output
                    return $ Right $ skeleton <> outputSkel <> mintSkel
                  _ -> return $ Left "Plutus version mismatch between transaction and validator script!"

  SUpdate stateRef fieldSpecs _constraints -> do
    liftIO $ putStrLn $ "Processing Update for state reference: " ++ show stateRef
    existingStateResult <- resolveStateRefSingleton (SAppInstance appSpec' instanceParams') stateRef params paramNames networkId providers
    case existingStateResult of
      Left err -> return $ Left err
      Right (existingUtxoRef, existingUtxo) -> do
        liftIO $ putStrLn $ "Found existing UTxO to update: " ++ show existingUtxoRef
        let stateType = extractStateTypeFromRef stateRef
        existingDatumResult <- liftIO $ extractExistingDatum existingUtxo stateType
        case existingDatumResult of
          Left err -> return $ Left err
          Right existingDatum -> do
            updatedDatumResult <- buildUpdatedDatumFromFieldSpecs stateType fieldSpecs params paramNames existingDatum derivationContext networkId providers
            case updatedDatumResult of
              Left err -> return $ Left err
              Right updatedDatum -> do
                liftIO $ putStrLn $ "🎯 Updated datum: " ++ show updatedDatum
                case findValidatorManagingState appSpec' (SomeStateType stateType) of
                  Left err -> return $ Left $ "No validator found for state: " <> err
                  Right (SomeValidator (validator :: SValidator v)) -> do
                    let validatorName = getValidatorNameFromSingleton validator
                    liftIO $ putStrLn $ "📦 Found validator: " ++ show validatorName

                    paramBuildResult <- case findParameterDerivationFor validatorName appSpec' of
                      Nothing -> return $ buildParamsToValue (getValidatorParams validator) params paramNames instanceParams' Nothing
                      Just derivation -> do
                        derivedParamsResult <- liftIO $ resolveValidatorParameters validatorName derivation (SAppInstance appSpec' instanceParams') networkId providers
                        case derivedParamsResult of
                          Left err -> return $ Left err
                          Right derivedParams -> return $ buildParamsToValue (getValidatorParams validator) params paramNames instanceParams' (Just derivedParams)

                    case paramBuildResult of
                      Left paramErr -> return $ Left $ "Failed to build validator params: " <> paramErr
                      Right validatorParams -> do
                        let script = getValidatorScript @app (Proxy @v) validatorParams
                        let scriptAddr = addressFromScript networkId script
                        let spv = singPlutusVersion @pv
                        case (spv, scriptVersion script) of
                          (SingPlutusV3, SingPlutusV3) -> do
                            let redeemer = case redeemerPolicy of
                                  UseNamedRedeemer name -> redeemerFromPlutusData name
                                  _ -> redeemerFromPlutusData ()
                            let spendInput = mustHaveInput $ GYTxIn existingUtxoRef (GYTxInWitnessScript
                                  (GYBuildPlutusScriptInlined script)
                                  Nothing
                                  redeemer)
                            let existingValue = utxoValue existingUtxo
                            let gyDatum = datumFromPlutus' (BuiltinData (toData updatedDatum))
                            let baseOutput = mkGYTxOut scriptAddr existingValue gyDatum
                            let output = baseOutput & gyTxOutDatumL .~ GYTxOutUseInlineDatum
                            let outputSkel = mustHaveOutput output
                            return $ Right $ skeleton <> spendInput <> outputSkel
                          _ -> return $ Left "Plutus version mismatch between transaction and validator script!"

  SDelete stateRef _constraints -> do
    liftIO $ putStrLn $ "Processing Delete for: " ++ show (extractStateFromRef stateRef)
    existingStateResult <- resolveStateRefSingleton (SAppInstance appSpec' instanceParams') stateRef params paramNames networkId providers
    case existingStateResult of
      Left err -> return $ Left err
      Right (existingUtxoRef, _existingUtxo) -> do
        liftIO $ putStrLn $ "Found UTxO to delete: " ++ show existingUtxoRef
        let stateType = extractStateTypeFromRef stateRef
        case findValidatorManagingState appSpec' (SomeStateType stateType) of
          Left err -> return $ Left $ "No validator found for state: " <> err
          Right (SomeValidator (validator :: SValidator v)) -> do
            let validatorName = getValidatorNameFromSingleton validator
            paramBuildResult <- case findParameterDerivationFor validatorName appSpec' of
              Nothing -> return $ buildParamsToValue (getValidatorParams validator) params paramNames instanceParams' Nothing
              Just derivation -> do
                derivedParamsResult <- liftIO $ resolveValidatorParameters validatorName derivation (SAppInstance appSpec' instanceParams') networkId providers
                case derivedParamsResult of
                  Left err -> return $ Left err
                  Right derivedParams -> return $ buildParamsToValue (getValidatorParams validator) params paramNames instanceParams' (Just derivedParams)

            case paramBuildResult of
              Left paramErr -> return $ Left $ "Failed to resolve validator parameters for delete: " <> paramErr
              Right validatorParams -> do
                let script = getValidatorScript @app (Proxy @v) validatorParams
                let spv = singPlutusVersion @pv

                case (spv, scriptVersion script) of
                  (SingPlutusV3, SingPlutusV3) -> do
                    let redeemer = case redeemerPolicy of
                          UseNamedRedeemer name -> redeemerFromPlutusData name
                          _ -> redeemerFromPlutusData ()
                    let buildPlutusScript :: GYBuildPlutusScript 'PlutusV3 = GYBuildPlutusScriptInlined script
                    let spendInput = mustHaveInput $ GYTxIn existingUtxoRef (GYTxInWitnessScript
                          buildPlutusScript
                          Nothing
                          redeemer)

                    let burnSkel = case stateIdentifier stateType of
                          TokenIdentified OwnPolicy stTokenName tokenQuantity ->
                            let burnQuantity = -tokenQuantity
                            in mustMint (GYBuildPlutusScript buildPlutusScript) redeemer stTokenName burnQuantity
                          _ -> mempty
                          
                    let cleanedSkeleton = case gytxRefIns skeleton of
                          GYTxSkeletonRefIns refInsSet ->
                            let newRefInsSet = delete existingUtxoRef refInsSet
                            in skeleton { gytxRefIns = GYTxSkeletonRefIns newRefInsSet }
                          GYTxSkeletonNoRefIns -> skeleton

                    return $ Right $ cleanedSkeleton <> spendInput <> burnSkel
                  _ -> return $ Left "Plutus version mismatch between transaction and validator script for Delete operation!"

  SReference stateRef _constraints -> do
    liftIO $ putStrLn $ "Processing Reference for: " ++ show (extractStateFromRef stateRef)
    refResult <- resolveStateRefSingleton (SAppInstance appSpec' instanceParams') stateRef params paramNames networkId providers
    case refResult of
      Left err -> return $ Left err
      Right (utxoRef', utxo) -> do
        liftIO $ putStrLn $ "Found UTxO to reference: " ++ show utxoRef'
        ctx <- get
        let refKey = pack $ show stateRef
        let statedUtxo = SomeStatedUTxO (extractStateTypeFromRef stateRef) utxo
        let updatedCtx = ctx { tbcResolvedRefs = insert refKey statedUtxo (tbcResolvedRefs ctx) }
        put updatedCtx
        let refSkel = mustHaveRefInput utxoRef'
        return $ Right $ skeleton <> refSkel

-- ============================================================================
-- 4. DATUM BUILDING FUNCTIONS
-- ============================================================================

-- | Builds a datum value ('GetStateData st') for a 'Create' operation using generics.
-- Iterates through the record fields and resolves each based on the 'SFieldSpecList'.
buildDatumFromFieldSpecs ::
  forall st fields params pv.
  ( StateRepresentable st
  , Generic (GetStateData st)
  , GBuildDatumFromSpecs (Rep (GetStateData st))
  ) =>
  SStateType st ->
  SFieldSpecList fields ->
  SParamTuple params ->
  [Text] ->
  DerivationContext ->
  GYNetworkId ->
  GYProviders ->
  TxBuilder pv (Either Text (GetStateData st))
buildDatumFromFieldSpecs stateType fieldSpecs params actionParamNames derivationContext networkId providers = do
  liftIO $ putStrLn $ "Building datum for: " ++ show (getStateName stateType)

  genericRep <- gBuildDatumFromSpecs fieldSpecs params actionParamNames derivationContext networkId providers Nothing
  case genericRep of
    Left err -> return $ Left err
    Right rep -> do
      -- Convert the generic representation back to the concrete data type.
      let datum = to rep
      liftIO $ putStrLn $ "Datum build successful: " ++ show datum
      return $ Right datum

-- | Builds an updated datum value ('GetStateData st') for an 'Update' operation using generics.
-- Similar to 'buildDatumFromFieldSpecs' but takes the existing datum (as 'Maybe (Rep...)')
-- to handle 'Preserve' specifications.
buildUpdatedDatumFromFieldSpecs ::
  forall st fields params pv.
  ( StateRepresentable st
  , Generic (GetStateData st)
  , GBuildDatumFromSpecs (Rep (GetStateData st))
  ) =>
  SStateType st ->
  SFieldSpecList fields ->
  SParamTuple params ->
  [Text] ->
  GetStateData st ->  -- ^ Existing datum value.
  DerivationContext ->
  GYNetworkId ->
  GYProviders ->
  TxBuilder pv (Either Text (GetStateData st))
buildUpdatedDatumFromFieldSpecs stateType fieldSpecs params actionParamNames existingDatum derivationContext networkId providers = do
  liftIO $ putStrLn $ "Building updated datum for: " ++ show (getStateName stateType)

  genericRep <- gBuildDatumFromSpecs fieldSpecs params actionParamNames derivationContext networkId providers (Just (from existingDatum))
  case genericRep of
    Left err -> return $ Left err
    Right rep -> do
      let updatedDatum = to rep
      liftIO $ putStrLn "✅ Successfully built updated datum"
      return $ Right updatedDatum

-- ============================================================================
-- 5. HELPER FUNCTIONS
-- ============================================================================

-- | Resolves an 'SStateRef' singleton to the actual on-chain UTxO ('GYTxOutRef', 'GYUTxO').
-- Handles different reference strategies ('TypedTheOnlyInstance', 'TypedUniqueWhere', 'TypedByLabel', etc.).
-- Queries the blockchain via 'GYProviders'.
resolveStateRefSingleton ::
  forall st ref app params pv.
  ( StateRepresentable st
  , AppValidatorScripts app
  , AppSpec app
  , Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
  , Generic (GetStateData st)
  , GExtractField (Rep (GetStateData st))
  , SingPlutusVersionI pv
  ) =>
  SAppInstance app ->
  SStateRef st ref ->
  SParamTuple params -> -- ^ Action parameters (used for predicates).
  [Text] -> -- ^ Action parameter names.
  GYNetworkId ->
  GYProviders ->
  TxBuilder pv (Either Text (GYTxOutRef, GYUTxO)) -- ^ The found UTxO Ref and UTxO or error.
resolveStateRefSingleton appInstance stateRef params paramNames networkId providers = do
  let (SAppInstance appSpec' instanceParams') = appInstance
  case stateRef of
    -- Case 1: Reference by Label (resolved from TxBuilderContext).
    STypedByLabel _ labelProxy -> do
        let labelText = pack $ symbolVal labelProxy
        ctx <- get
        case lookup labelText (toList $ tbcLetResults ctx) of
            Just (ORReference (SomeStatedUTxO _ utxo)) -> return $ Right (utxoRef utxo, utxo)
            _ -> return $ Left $ "Could not resolve 'Let' binding with label: " <> labelText

    _ -> liftIO $ do -- The rest of the logic remains in IO
      case findValidatorManagingState appSpec' (SomeStateType (extractStateTypeFromRef stateRef)) of
        Left err -> return $ Left err
        Right (SomeValidator (validator :: SValidator v)) -> do
          let validatorName = getValidatorNameFromSingleton validator
          paramBuildResult <- case findParameterDerivationFor validatorName appSpec' of
            Nothing -> return $ buildParamsToValue (getValidatorParams validator) STupleNil [] instanceParams' Nothing
            Just derivation -> do
              derivedParamsResult <- liftIO $ resolveValidatorParameters validatorName derivation appInstance networkId providers
              case derivedParamsResult of
                Left err -> return $ Left err
                Right derivedParams -> return $ buildParamsToValue (getValidatorParams validator) STupleNil [] instanceParams' (Just derivedParams)

          case paramBuildResult of
            Left err -> return $ Left err
            Right validatorParams -> do
              let script = getValidatorScript @app (Proxy @v) validatorParams
              let validatorAddr = addressFromScript networkId script

              case stateRef of
                -- Find the single unique instance (uses token name if available).
                STypedTheOnlyInstance stateType ->
                  resolveUniqueState validatorAddr stateType networkId providers
                -- Find the unique instance matching a predicate.
                STypedUniqueWhere stateType predicate -> do
                  utxos <- runGYTxQueryMonadIO networkId providers $ utxosAtAddress validatorAddr Nothing
                  matchingUtxos <- filterM (utxoMatchesPredicate stateType predicate params paramNames) (utxosToList utxos)
                  case matchingUtxos of
                    [utxo] -> return $ Right (utxoRef utxo, utxo)
                    [] -> return $ Left "No UTxOs found matching the predicate"
                    _ -> return $ Left "Multiple UTxOs found matching the predicate for a 'UniqueWhere' reference"
                STypedAnyWhere stateType predicate -> do
                  utxos <- runGYTxQueryMonadIO networkId providers $ utxosAtAddress validatorAddr Nothing
                  matchingUtxos <- filterM (utxoMatchesPredicate stateType predicate params paramNames) (utxosToList utxos)
                  case matchingUtxos of
                    [] -> return $ Left "No UTxOs found matching the predicate for 'AnyWhere'"
                    (utxo:_) -> return $ Right (utxoRef utxo, utxo) -- Return the first one found
                _ -> return $ Left "This state reference type is not yet implemented for blockchain resolution"

utxoMatchesPredicate :: (StateRepresentable st, Generic (GetStateData st), GExtractField (Rep (GetStateData st)))
                     => SStateType st
                     -> SPredicate (pred :: TypedPredicate st)
                     -> SParamTuple params
                     -> [Text] 
                     -> GYUTxO
                     -> IO Bool
utxoMatchesPredicate stateType predicate params paramNames utxo =
  case utxoOutDatum utxo of
    -- Only considers inline datums.
    GYOutDatumInline datum ->
      -- Evaluate the predicate against the datum.
      case evaluatePredicate stateType predicate params paramNames datum of
        Right True -> return True
        _          -> return False
    _ -> return False

-- | (Internal) Resolves the unique instance of a state at a validator address.
-- Primarily uses the state's 'TokenIdentified' 'OwnPolicy' token if available.
resolveUniqueState ::
  forall st.
  StateRepresentable st =>
  GYAddress ->
  SStateType st ->
  GYNetworkId ->
  GYProviders ->
  IO (Either Text (GYTxOutRef, GYUTxO))
resolveUniqueState validatorAddr stateType networkId providers = do
  case stateIdentifier stateType of
    -- If identified by a unique token minted by this validator...
    TokenIdentified OwnPolicy stTokenName _tokenQuantity -> do
      allUtxos <- runGYTxQueryMonadIO networkId providers $ utxosAtAddress validatorAddr Nothing

      let matchingUtxos = filter (hasToken stTokenName) (utxosToList allUtxos)
      case matchingUtxos of
        [utxo] -> return $ Right (utxoRef utxo, utxo)
        [] -> return $ Left $ "No UTxOs found with token: " <> pack (show stTokenName)
        _ -> return $ Left $ "Multiple UTxOs found with token: " <> pack (show stTokenName)

    TokenIdentified (ExternalPolicy _) _ _ ->
      return $ Left "External policy tokens not supported in refactored version"

    AggregateAsset _ ->
      return $ Left $ "Cannot resolve a unique instance for an AggregateAsset state type: " <> getStateName stateType

-- | (Internal) Checks if a UTxO's value contains a specific token name (with quantity > 0).
hasToken :: GYTokenName -> GYUTxO -> Bool
hasToken targetTokenName utxo =
  let value = utxoValue utxo
      tokens = valueToList value
  in any checkToken tokens
  where
    checkToken :: (GYAssetClass, Integer) -> Bool
    -- Check if asset is a token, matches the name, and has positive quantity.
    checkToken (GYToken _ tokenName, quantity) = tokenName == targetTokenName && quantity > 0
    checkToken (GYLovelace, _) = False

-- | Extracts and parses the inline datum from a 'GYUTxO' into the expected state data type.
extractExistingDatum ::
  forall st.
  StateRepresentable st =>
  GYUTxO ->
  SStateType st ->
  IO (Either Text (GetStateData st))
extractExistingDatum utxo (SStateType :: SStateType st) = do
  case utxoOutDatum utxo of
    GYOutDatumInline gyDatum -> do
      let builtinData = toBuiltinData gyDatum
      case fromBuiltinData builtinData :: Maybe (GetStateData st) of
        Just stateData -> return $ Right stateData
        Nothing -> return $ Left "Failed to parse existing state data"
    _ -> return $ Left "Existing UTxO does not have inline datum"

-- | Extracts the 'SStateType' singleton from an 'SStateRef'. (Duplicate from elsewhere, consider consolidating)
extractStateTypeFromRef :: SStateRef st ref -> SStateType st
extractStateTypeFromRef (STypedTheOnlyInstance stateType) = stateType
extractStateTypeFromRef (STypedUniqueWhere stateType _) = stateType
extractStateTypeFromRef (STypedAny stateType) = stateType
extractStateTypeFromRef (STypedAnyWhere stateType _) = stateType
extractStateTypeFromRef (STypedByLabel stateType _) = stateType

-- | (Internal) Extracts the parameter names required by the inner operation of a 'Map' step.
-- Used to correctly pass parameters derived from the mapped list item.
extractInnerParamNames :: SOperation op -> [Text]
extractInnerParamNames op = case op of
  -- For Create/Update, extract names from 'ParamValue' specs.
  SCreate _ fields _ -> extractFromFields fields
  SUpdate _ fields _ -> extractFromFields fields
  -- For Delete, extract names used in the predicate of the reference.
  SDelete ref _      -> extractFromRef ref
  -- Reference/other ops don't usually have inner params in Map context.
  _                  -> []
  where
    -- Recursively scan FieldSpecList for ParamValue
    extractFromFields :: SFieldSpecList fields -> [Text]
    extractFromFields SFSNil = []
    extractFromFields (SFSCons (SSetTo _ (SParamValue nameProxy)) rest) =
      pack (symbolVal nameProxy) : extractFromFields rest
    extractFromFields (SFSCons _ rest) = extractFromFields rest

    -- Extract parameter names used within predicates
    extractFromRef :: SStateRef st ref' -> [Text]
    extractFromRef (STypedUniqueWhere _ pred') = extractFromPredicate pred'
    extractFromRef _ = []

    -- Recursively scan predicate structure for ParamValue
    extractFromPredicate :: SPredicate pred' -> [Text]
    extractFromPredicate (SFieldEquals _ (SParamValue nameProxy)) = [pack (symbolVal nameProxy)]
    extractFromPredicate (SAnd p1 p2) =
      extractFromPredicate p1 ++ extractFromPredicate p2
    extractFromPredicate _ = []

-- | Resolves a 'STypedValue' singleton into a concrete 'GYValue', suitable for transaction outputs.
-- Handles various value sources like parameters, state fields, literals, and arithmetic operations.
-- Requires 'TxBuilder' context for 'SCurrentTime' and 'SStateFieldValue'.
resolveValueForTx ::
  forall value params app pv.
  (AppSpec app, AppValidatorScripts app, SingPlutusVersionI pv) =>
  STypedValue value -> -- ^ The singleton value specification.
  SParamTuple params -> -- ^ Action parameters tuple.
  [Text] -> -- ^ Action parameter names.
  SAppInstance app -> -- ^ Application instance context (unused currently).
  GYNetworkId -> -- ^ Network ID (unused currently).
  GYProviders -> -- ^ Blockchain providers (unused currently).
  TxBuilder pv (Either Text GYValue) -- ^ Resulting 'GYValue' or error.
resolveValueForTx valueSpec params paramNames _appInstance _networkId _providers = do
    -- Use recursive helper to resolve to SomeFieldValue first
    eSomeValue <- resolveValue' valueSpec
    case eSomeValue of
        Left err -> return $ Left err
        -- Currently assumes the final value needed for output is always Integer (Lovelace).
        -- TODO: Extend to handle specific AssetClasses.
        Right (SomeFieldValue val) ->
            case cast val of
                Just (lovelace :: Integer) -> return $ Right $ valueFromLovelace lovelace
                Nothing -> return $ Left "Resolved value for transaction output was not an Integer."
  where
    -- The recursive helper function for resolving TypedValues.
    resolveValue' :: forall v. STypedValue v -> TxBuilder pv (Either Text SomeFieldValue)
    resolveValue' (SStateFieldValue labelProxy' fieldProxy') = do
        let labelText' = pack $ symbolVal labelProxy'
        let fieldText' = pack $ symbolVal fieldProxy'
        ctx <- get
        case lookup labelText' (toList $ tbcLetResults ctx) of
            Just (ORReference (SomeStatedUTxO stype utxo)) ->
                case extractFieldFromReferencedUTxO @Integer fieldText' (SomeStatedUTxO stype utxo) of
                    Right intValue -> return $ Right $ SomeFieldValue intValue
                    Left err -> return $ Left err
            _ -> return $ Left $ "Could not resolve StateFieldValue for payment: " <> labelText' <> "." <> fieldText'
    resolveValue' (SParamValue paramProxy) =
      let paramName = pack $ symbolVal paramProxy
      in case extractParamByNameDirectWithProxy (Proxy @Integer) paramName paramNames params of
          Left err -> return $ Left $ "Failed to extract payment value from parameter: " <> err
          Right lovelace -> return $ Right $ SomeFieldValue lovelace
    resolveValue' (SIntValue natProxy) = return $ Right $ SomeFieldValue (natVal natProxy)

    -- Arithmetic cases that recursively call resolveValue'
    resolveValue' (SSubtractValue v1 v2) = applyArithmetic (-) v1 v2
    resolveValue' (SMultiplyValue v1 v2) = applyArithmetic (*) v1 v2
    resolveValue' (SDivideValue v1 v2) = applyArithmetic div v1 v2
    resolveValue' _ = return $ Left "Unsupported TypedValue for payment."

    -- Helper for arithmetic operations (assumes Integer operands).
    applyArithmetic :: (Integer -> Integer -> Integer) -> STypedValue v1 -> STypedValue v2 -> TxBuilder pv (Either Text SomeFieldValue)
    applyArithmetic op v1 v2 = do
        eVal1 <- resolveValue' v1
        eVal2 <- resolveValue' v2
        case (eVal1, eVal2) of
            (Right (SomeFieldValue val1), Right (SomeFieldValue val2)) ->
                case (cast val1, cast val2) of
                    (Just (i1 :: Integer), Just (i2 :: Integer)) -> return $ Right $ SomeFieldValue (op i1 i2)
                    _ -> return $ Left "Arithmetic operation encountered a non-Integer value."
            (Left err, _) -> return $ Left err
            (_, Left err) -> return $ Left err

-- | Resolves a 'STypedValue' singleton into a concrete Plutus 'Address'.
-- Currently only supports resolving from 'SParamValue' or 'SStateFieldValue'.
-- Runs within 'TxBuilder' monad because 'SStateFieldValue' requires context lookup.
resolveAddressForTx ::
  forall value params.
  STypedValue value ->
  SParamTuple params ->
  [Text] ->
  Either Text Address
resolveAddressForTx (SParamValue paramProxy) params paramNames =
  let paramName = pack $ symbolVal paramProxy
  in case extractParamByNameDirectWithProxy (Proxy @Address) paramName paramNames params of
    Left err -> Left $ "Failed to extract Address from parameter: " <> err
    Right addr -> Right addr
resolveAddressForTx _ _ _ = Left "Unsupported TypedValue for resolving an Address. Only ParamValue is supported."