{-|
Module      : Modsefa.Core.IR.Compiler
Description : Compiles Modsefa specifications into Intermediate Representation (IR).
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module provides the 'compileIR' function, which serves as the bridge
between the type-level, singleton-based representation of a Modsefa application
('Modsefa.Core.Singletons.Types.SAppSpec') and the value-level Intermediate Representation
('Modsefa.Core.IR.Types.AppIR').

The compiler traverses the singleton structure, extracts relevant information
(validator names, managed states, action details, constraints, parameters),
performs analysis (like identifying actions relevant to each validator and generating
instance consistency checks), and constructs the corresponding IR data structures.
The resulting IR is then used by subsequent stages like code generation.
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Compiles a type-level `SAppSpec` into a value-level Intermediate Representation (IR).
--
-- This module is the bridge between the singleton-based specification of the
-- application and the backend-agnostic IR. It replaces the ad-hoc analysis
-- that was previously done in `Modsefa.CodeGen.Analysis.Branches`.
module Modsefa.Core.IR.Compiler
  ( compileIR
  ) where

import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy)
import Data.Text (Text, pack)
import Data.Typeable (typeRep)
import GHC.TypeLits (natVal, symbolVal)

import GeniusYield.Types (PlutusVersion(..))

import Modsefa.Core.Foundation 
  ( AppSpec, Params, SStateType(SStateType), ValidatorSpec
  )
import Modsefa.Core.Singletons
  ( SActionSpec(..), SActionStep(..), SActionStepList(..), SActionTransition(..)
  , SActionTransitionList(..), SAppSpec(..), SCollectionConstraint(..)
  , SCollectionConstraintList(..), SConstraint(..), SConstraintList(..)
  , SDerivationSource(SValidatorAddress), SFieldSpec(..), SFieldSpecList(..)
  , SOperation(..), SParamDerivation(SDeriveParam), SParamDerivationList
  , SParamList(..), SPlutusVersion(..), SStateRef, STypedValue(..), SValidator(..)
  , SValidatorList(..), SomeStateType(..), SomeValidator(SomeValidator)
  , extractStateFromRef, extractStateName, extractStateNamesFromStateList
  , extractStateTypeFromRef', findValidatorManagingState, getStateName
  , getValidatorNameFromSingleton, stateInList, validatorManagesState
  )
import Modsefa.Core.Transaction 
  (SomeParameterDerivation(SomeParameterDerivation), findInDerivationList
  )

import Modsefa.Core.IR.Types
  ( ActionIR(..), AppIR(..), BatchOperationIR(..), CollectionConstraintIR(..)
  , ConstraintIR(..), FieldValueIR(..)
  , InstanceCheckIR(AddressMatchesParamIR, amprParamName, amprReferenceState)
  , OperationIR(..), PubKeyHashIR(FromActionParamPKH, FromStateFieldPKH)
  , ValidatorIR(..)
  )


-- | Existential wrapper for 'SOperation', used internally during compilation.
data SomeOperation where
  SomeOperation :: SOperation op -> SomeOperation

-- | Compiles a value-level 'SAppSpec' singleton into an 'AppIR'.
-- This is the main entry point for converting the type-safe specification
-- into the intermediate representation used for code generation.
compileIR :: forall app. AppSpec app 
          => Text -- ^ The overall application name (not derived from types).
          -> SAppSpec app -- ^ The singleton representation of the application specification.
          -> AppIR -- ^ The resulting Intermediate Representation.
compileIR appName spec@(SAppSpec validators _ _ transitions _ derivations) = AppIR
  { appIRName       = appName
  , appIRValidators = compileValidators spec validators transitions derivations 
  }

-- | Compiles an 'SValidatorList' into a list of 'ValidatorIR's.
compileValidators :: SAppSpec app -> SValidatorList vs -> SActionTransitionList app ts -> SParamDerivationList ds -> [ValidatorIR]
compileValidators spec validators transitions derivations = go validators
 where
  -- Helper function to recurse through the validator list
  go :: SValidatorList vs_go -> [ValidatorIR]
  go SVNil = []
  go (SVCons v rest) =
   let validator = compileValidator spec v transitions derivations -- Compile head
   in validator : go rest -- Compile tail

-- | Compiles a single 'SValidator' into a 'ValidatorIR'.
-- This involves extracting validator properties and finding all relevant actions.
compileValidator :: forall v app ts ds. ValidatorSpec v
                 => SAppSpec app
                 -> SValidator v
                 -> SActionTransitionList app ts
                 -> SParamDerivationList ds
                 -> ValidatorIR
compileValidator appSpec v@(SValidator params managedStates pv _) transitions derivations =
  let vName = getValidatorNameFromSingleton v
  in ValidatorIR
      { validatorIRName    = vName
      , validatorIRParams  = []
      , validatorIRVersion = case pv of SPlutusV1 -> PlutusV1; SPlutusV2 -> PlutusV2; SPlutusV3 -> PlutusV3
      , validatorIRManagedStates = extractStateNamesFromStateList managedStates
      , validatorIRActions = findActionsForValidator appSpec v params derivations transitions
      }

-- | Scans the application's 'ActionTransitions' to find and compile all 'ActionIR's
-- relevant to a specific validator. An action is relevant if it operates on a state
-- managed by the validator or contains a constraint targeting the validator.
findActionsForValidator :: forall v app ts ds. ValidatorSpec v
                        => SAppSpec app
                        -> SValidator v
                        -> SParamList (Params v) -- ^ Parameters of the current validator.
                        -> SParamDerivationList ds -- ^ All app derivation rules.
                        -> SActionTransitionList app ts -- ^ All app transitions.
                        -> [ActionIR]
findActionsForValidator appSpec v vParams derivations = go
  where
    go :: SActionTransitionList app ts' -> [ActionIR]
    go SATLNil = []
    go (SATLCons (SActionTransition actionSpec _ _) rest) =
      -- Check if the action affects the current validator
      if actionAffectsValidator v actionSpec
        -- If relevant, compile the action spec into ActionIR
        then compileAction appSpec vParams derivations actionSpec : go rest
        else go rest

-- | Determines if an 'SActionSpec' is relevant to a given 'SValidator'.
-- Checks if any step operates on a managed state or if any constraint targets the validator.
actionAffectsValidator :: forall v app spec. ValidatorSpec v => SValidator v -> SActionSpec app spec -> Bool
actionAffectsValidator v (SActionSpec _ steps constraints _) =
  stepsAffect v steps || constraintsAffect v constraints
  where
    -- Checks if any step in the list affects the validator
    stepsAffect :: SValidator v -> SActionStepList ss -> Bool
    stepsAffect _ ASSLNil = False
    stepsAffect val (ASSLCons s rest) = stepAffectsValidator val s || stepsAffect val rest

    -- Checks if any constraint in the list affects the validator
    constraintsAffect :: SValidator v -> SConstraintList cs -> Bool
    constraintsAffect _ SCLNil = False
    constraintsAffect val (SCLCons c rest) = constraintAffectsValidator val c || constraintsAffect val rest

-- | Checks if a single 'SActionStep' affects the validator (delegates to 'opAffectsValidator').
stepAffectsValidator :: SValidator v -> SActionStep s -> Bool
stepAffectsValidator v step = case step of
  SOp op -> opAffectsValidator v op
  SLet _ op -> opAffectsValidator v op
  SMap op _ _ -> opAffectsValidator v op

-- | Checks if an 'SOperation' affects the validator. True if the operation
-- (Create, Update, Delete) targets a state managed by the validator. 'Reference' operations do not affect the managing validator directly.
opAffectsValidator :: SValidator v -> SOperation op -> Bool
opAffectsValidator (SValidator _ managedStates _ _) op =
  case op of
    SReference _ _ -> False
    _ -> stateInList (getOpStateType op) managedStates

-- | Checks if an 'SConstraint' directly affects or targets the validator.
-- E.g., 'SMustSpendValidatorParam' matching the validator's name, or 'SMustWithdrawFromAggregateState'
-- targeting a state managed by this validator.
constraintAffectsValidator :: forall v c. ValidatorSpec v => SValidator v -> SConstraint c -> Bool
constraintAffectsValidator v (SMustSpendValidatorParam vNameProxy _) =
  pack (symbolVal vNameProxy) == getValidatorNameFromSingleton v
constraintAffectsValidator v (SMustWithdrawFromAggregateState st _ _) =
  validatorManagesState v (SomeStateType st)
constraintAffectsValidator _ _ = False

-- | Compiles an 'SActionSpec' into an 'ActionIR'.
-- Extracts the name, compiles operations and constraints, and generates instance consistency checks.
compileAction :: SAppSpec app 
              -> SParamList vParams -- ^ Parameters of the current validator being compiled.
              -> SParamDerivationList ds -- ^ All app derivation rules.
              -> SActionSpec app spec 
              -> ActionIR
compileAction appSpec vParams derivations actionSpec@(SActionSpec nameProxy steps constraints _) = ActionIR
  { actionIRName        = pack $ symbolVal nameProxy
  , actionIROperations  = compileOperations steps
  , actionIRConstraints = compileConstraints constraints ++ generateInstanceChecks appSpec vParams derivations actionSpec
  }

-- | Compiles an 'SActionStepList' into a list of 'OperationIR'.
compileOperations :: SActionStepList ss -> [OperationIR]
compileOperations = go
  where
    go :: SActionStepList ss' -> [OperationIR]
    go ASSLNil = []
    go (ASSLCons step rest) = compileStep step ++ go rest

    -- Compiles a single SActionStep into OperationIR(s)
    compileStep :: SActionStep s -> [OperationIR]
    compileStep (SOp op) = [compileOperation op]
    compileStep (SLet labelProxy op) =
      case op of
        SReference ref _ ->
          [ReferenceOpIR
            { opIRStateName = extractStateFromRef ref
            , opIRLabel     = pack (symbolVal labelProxy)
            }]
        _ -> [compileOperation op]
    compileStep (SMap op paramProxy constraints) =
      [BatchOpIR
        { opIRBatchOperation  = compileBatchOperation op
        , opIRCollectionParam = pack (symbolVal paramProxy)
        , opIRConstraints     = compileCollectionConstraints constraints
        }]

-- | Compiles a single 'SOperation' (excluding 'SReference' outside 'SLet') into an 'OperationIR'.
compileOperation :: SOperation op -> OperationIR
compileOperation (SCreate st fields _) = CreateOpIR
  { opIRStateName = getStateName st
  , opIRFields    = compileFields fields
  }
compileOperation (SUpdate ref fields _) = UpdateOpIR
  { opIRStateName = extractStateFromRef ref
  , opIRFields    = compileFields fields
  }
compileOperation (SDelete ref _) = DeleteOpIR
  { opIRStateName = extractStateFromRef ref
  }
compileOperation (SReference _ _) =
  error "compileOperation: Encountered a 'Reference' outside of a 'Let' binding. This is not supported."

-- | Compiles an 'SFieldSpecList' into a list of '(FieldName, FieldValueIR)' tuples.
compileFields :: SFieldSpecList fs -> [(Text, FieldValueIR)]
compileFields = go
  where
    go :: SFieldSpecList fs' -> [(Text, FieldValueIR)]
    go SFSNil = []
    go (SFSCons spec rest) = compileField spec : go rest

    -- Compiles a single SFieldSpec
    compileField :: SFieldSpec f -> (Text, FieldValueIR)
    compileField (SSetTo nameProxy val) = (pack $ symbolVal nameProxy, compileValue val)
    compileField (SPreserve nameProxy) = (pack $ symbolVal nameProxy, FromInputField (pack $ symbolVal nameProxy))

-- | Compiles an 'STypedValue' singleton into a 'FieldValueIR' value.
compileValue :: STypedValue v -> FieldValueIR
compileValue (SParamValue nameProxy) = FromActionParam (pack $ symbolVal nameProxy)
compileValue (SEnumValue (tProxy :: Proxy t) s) = FromEnum (pack (show (typeRep tProxy))) (pack (symbolVal s))
compileValue (SIntValue n) = FromInt (natVal n)
compileValue (SStateFieldValue labelProxy fieldProxy) =
  FromStateField (pack $ symbolVal labelProxy) (pack $ symbolVal fieldProxy)
compileValue SCurrentTime = CurrentTimeIR
compileValue (SAddValue v1 v2) = AddValueIR (compileValue v1) (compileValue v2)
compileValue (SSubtractValue v1 v2) = SubtractValueIR (compileValue v1) (compileValue v2)
compileValue (SMultiplyValue v1 v2) = MultiplyValueIR (compileValue v1) (compileValue v2)
compileValue (SDivideValue v1 v2) = DivideValueIR (compileValue v1) (compileValue v2)

-- | Compiles an 'SConstraintList' into a list of 'ConstraintIR', filtering out constraints
-- handled implicitly or not relevant to on-chain validation (like 'SMustSpendValidatorParam').
compileConstraints :: SConstraintList cs -> [ConstraintIR]
compileConstraints SCLNil = []
compileConstraints (SCLCons c rest) =
  case compileConstraint c of
    Just ir -> ir : compileConstraints rest -- Add compiled constraint if relevant
    Nothing -> compileConstraints rest -- Skip irrelevant constraint

-- | Compiles a single 'SConstraint' into a 'Maybe ConstraintIR'.
-- Returns 'Nothing' for constraints handled implicitly by operations or parameter spending.
compileConstraint :: SConstraint c -> Maybe ConstraintIR
compileConstraint (SMustBeSignedByState ref fieldProxy) =
  -- Compiles to MustBeSignedBy with FromStateFieldPKH source
  Just $ MustBeSignedBy $ FromStateFieldPKH (extractStateFromRef ref) (pack $ symbolVal fieldProxy)
compileConstraint (SMustBeSignedByParam nameProxy) =
  -- Compiles to MustBeSignedBy with FromActionParamPKH source
  Just $ MustBeSignedBy $ FromActionParamPKH (pack $ symbolVal nameProxy)
compileConstraint (SMustSpendActionParam nameProxy) =
  -- Compiles directly to MustSpendActionParamIR
  Just $ MustSpendActionParamIR (pack $ symbolVal nameProxy)
compileConstraint (SMustAddToAggregateState st val) =
  -- Compiles directly, includes state name and compiled value
  Just $ MustAddToAggregateStateIR (getStateName st) (compileValue val)
compileConstraint (SMustWithdrawFromAggregateState st val addr) =
  -- Compiles directly, includes state name and compiled value/address
  Just $ MustWithdrawFromAggregateStateIR (getStateName st) (compileValue val) (compileValue addr)
compileConstraint (SMustSpendValidatorParam _ _) = Nothing
compileConstraint (SMustNotExist _) = Nothing

-- | Helper to get the 'SomeStateType' existential wrapper from an 'SOperation'.
getOpStateType :: SOperation op -> SomeStateType
getOpStateType (SCreate stype _ _) = SomeStateType stype
getOpStateType (SUpdate ref _ _)   = SomeStateType (extractStateTypeFromRef' ref)
getOpStateType (SDelete ref _)     = SomeStateType (extractStateTypeFromRef' ref)
getOpStateType (SReference ref _)  = SomeStateType (extractStateTypeFromRef' ref)

-- | Compiles the inner operation of a 'Map' step into a 'BatchOperationIR'.
-- Currently supports only Create and Delete.
compileBatchOperation :: SOperation op -> BatchOperationIR
compileBatchOperation (SCreate st fields _) = BatchCreateIR
  { batchOpIRStateName = getStateName st
  , batchOpIRFields    = compileFields fields
  }
compileBatchOperation (SDelete ref _) = BatchDeleteIR
  { batchOpIRStateName = extractStateFromRef ref
  }
compileBatchOperation _ = error "Only Create and Delete operations are supported in a Map"

-- | Compiles an 'SCollectionConstraintList' into a list of 'CollectionConstraintIR'.
compileCollectionConstraints :: SCollectionConstraintList cs -> [CollectionConstraintIR]
compileCollectionConstraints SCCNil = []
compileCollectionConstraints (SCCCons c rest) = compileCollectionConstraint c : compileCollectionConstraints rest

-- | Compiles a single 'SCollectionConstraint' into a 'CollectionConstraintIR'.
compileCollectionConstraint :: SCollectionConstraint c -> CollectionConstraintIR
compileCollectionConstraint (SMustHaveUniqueField fieldProxy) = MustHaveUniqueFieldIR (pack $ symbolVal fieldProxy)

-- | Generates implicit 'MustCheckInstance' constraints ('InstanceCheckIR') based on
-- 'Reference' operations within an action and the parameter derivation rules of the application.
-- Ensures that referenced states belong to the correct application instance, especially when
-- validators derive parameters (like addresses) from each other.
generateInstanceChecks :: SAppSpec app
                       -> SParamList vParams -- ^ Parameters of the validator being compiled.
                       -> SParamDerivationList ds -- ^ All app derivation rules.
                       -> SActionSpec app spec -- ^ The action being compiled.
                       -> [ConstraintIR]
generateInstanceChecks appSpec vParams derivations (SActionSpec _ steps _ _) =
  -- Check each step, filter Maybe results, and wrap valid checks in MustCheckInstance
  mapMaybe (checkStepForInstanceConstraint appSpec vParams derivations) (flattenSteps steps)
  where
    -- Extracts all operations, including those inside Let/Map, associating Let ops with their label
    flattenSteps :: SActionStepList ss -> [(Maybe Text, SomeOperation)]
    flattenSteps ASSLNil = []
    flattenSteps (ASSLCons (SOp op) rest) = (Nothing, SomeOperation op) : flattenSteps rest
    flattenSteps (ASSLCons (SLet label op) rest) = (Just (pack $ symbolVal label), SomeOperation op) : flattenSteps rest
    flattenSteps (ASSLCons (SMap op _ _) rest) = (Nothing, SomeOperation op) : flattenSteps rest

-- | Checks a single step (specifically 'Reference' ops) to see if an instance check is needed.
checkStepForInstanceConstraint :: SAppSpec app
                               -> SParamList vParams -- ^ Current validator's params
                               -> SParamDerivationList ds -- ^ All derivations
                               -> (Maybe Text, SomeOperation) -- ^ The step (label, operation)
                               -> Maybe ConstraintIR -- ^ 'Just MustCheckInstance' if needed, else 'Nothing'
checkStepForInstanceConstraint appSpec vParams derivations (_, SomeOperation op) = case op of
  SReference ref _constraints -> checkReference appSpec ref vParams derivations
  _ -> Nothing

-- | Analyzes a 'Reference' operation ('SStateRef') to determine if an 'InstanceCheckIR' is required.
-- An instance check is needed if:
-- 1. The *current* validator has an 'Address' parameter derived ('DeriveParam') from another validator ('ValidatorAddress').
-- 2. The state being referenced ('referencedStateRef') is managed by that *same source validator*.
-- This ensures the referenced state belongs to the same instance anchor as the current validator.
checkReference :: SAppSpec app
               -> SStateRef referencedState ref -- ^ The state reference being checked.
               -> SParamList vParams -- ^ Params of the validator containing this reference.
               -> SParamDerivationList ds -- ^ All app derivation rules.
               -> Maybe ConstraintIR -- ^ The specific check needed, or Nothing.
checkReference appSpec referencedStateRef vParams derivations = do
  -- 1. Find the derivation rule for the current validator's address parameter.
  addressParamName <- findFirstAddressParamName vParams
  derivation <- findInDerivationList addressParamName derivations

  -- 2. Check if the derivation source is a ValidatorAddress.
  case derivation of
    SomeParameterDerivation (SDeriveParam _ (SValidatorAddress sourceValidatorNameProxy)) -> do
        let sourceValidatorName = pack $ symbolVal sourceValidatorNameProxy

        -- 3. Get the SStateType singleton first
        let sStateType = extractStateTypeFromRef' referencedStateRef
        -- Pattern match using the SStateType constructor to bring the
        -- StateRepresentable constraint into scope for type 'st'.
        case sStateType of
          (SStateType :: SStateType st) -> do

            -- 4. Now 'StateRepresentable st' is in scope
            let referencedStateName = extractStateName (SStateType @st)

            -- 5. Find the validator managing the referenced state.
            -- findValidatorManagingState requires StateRepresentable st
            case findValidatorManagingState appSpec (SomeStateType (SStateType @st)) of
                Left _ -> Nothing -- Should not happen if prior type validation passed
                Right (SomeValidator managingValidator) -> do
                    let managingValidatorName = getValidatorNameFromSingleton managingValidator

                    -- 6. Compare the managing validator's name with the derivation source name.
                    if managingValidatorName == sourceValidatorName then
                        -- Match! Generate the IR constraint.
                        Just $ MustCheckInstance $ AddressMatchesParamIR
                          { amprReferenceState = referencedStateName
                          , amprParamName      = addressParamName
                          }
                    else
                        -- Referenced state is managed by a different validator than the derivation source.
                        Nothing
    _ -> Nothing -- Derivation doesn't involve a ValidatorAddress or parameter not found
    
-- | Finds the name ('Text') of the first parameter of type 'Address' in an 'SParamList'.
-- Returns 'Nothing' if no Address parameter is found.
-- Note: Relies on Typeable constraint within SPCons. Needs robust type checking.
findFirstAddressParamName :: SParamList ps -> Maybe Text
findFirstAddressParamName SPNil = Nothing
findFirstAddressParamName (SPCons nameProxy (pType :: Proxy t) rest) =
    -- Attempt to check if 't' is 'Address' using Typeable.
    -- This is slightly fragile; could use more robust type equality if available/needed.
    if show (typeRep pType) == "Address" || show (typeRep pType) == "V3.Address"
      then Just $ pack (symbolVal nameProxy)
      else findFirstAddressParamName rest