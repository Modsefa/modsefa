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
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Compiles a type-level `SAppSpec` into a value-level Intermediate Representation (IR).
--
-- This module is the bridge between the singleton-based specification of the
-- application and the backend-agnostic IR. It replaces the ad-hoc analysis
-- that was previously done in `Modsefa.CodeGen.Analysis.Branches`.
module Modsefa.Core.IR.Compiler
  ( compileIR
  , compileStateList
  ) where

import Data.Kind (Type)
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text, pack)
import Data.Typeable (typeRep)
import GHC.TypeLits (natVal, symbolVal)

import GeniusYield.Types (PlutusVersion(..))

import Modsefa.Core.Foundation
  ( AppSpec, DatumName, Params, StateSpec, TypedStateRef, ValidatorSpec
  )
import Modsefa.Core.Singletons
  ( SActionSpec(..), SActionStep(..), SActionStepList(..), SActionTransition(..)
  , SActionTransitionList(..), SAppSpec(..), SCollectionConstraint(..)
  , SCollectionConstraintList(..), SConstraint(..), SConstraintList(..)
  , SDatumField(..), SDatumFieldList(..), SDerivationSource(SValidatorAddress)
  , SFieldSpec(..), SFieldSpecList(..), SMappability(..), SOperation(..)
  , SParamDerivation(SDeriveParam), SParamDerivationList, SParamList(..)
  , SPlutusVersion(..), SPolicySource(..), SStateList(..), SStateRef, SStateSpec(..)
  , STypedValue(..), SValidator(..), SValidatorList(..), SomePolicySource(..)
  , SomeStateType(..), SomeValidator(SomeValidator), extractDatumName
  , extractPolicySource, extractStateFromRef, extractStateName
  , extractStateNamesFromStateList, extractStateTypeFromRef', extractTokenName
  , findValidatorManagingState, getValidatorNameFromSingleton, stateInList
  , validatorManagesState
  )
import Modsefa.Core.Transaction
  ( SomeParameterDerivation(SomeParameterDerivation), findInDerivationList
  )

import Modsefa.Core.IR.Types
  ( ActionIR(..), AppIR(..), BatchOperationIR(..), CollectionConstraintIR(..)
  , ConstraintIR(..), DatumFieldIR(..), FieldValueIR(..)
  , InstanceCheckIR(AddressMatchesParamIR, amprParamName, amprReferenceState)
  , OperationIR(..), PolicySourceIR(..)
  , PubKeyHashIR(FromActionParamPKH, FromStateFieldPKH), StateInfoIR(..)
  , ValidatorIR(..)
  )


-- ============================================================================
-- * INTERNAL TYPES
-- ============================================================================

-- | Existential wrapper for 'SOperation', used internally during compilation.
data SomeOperation where
  SomeOperation :: SOperation op -> SomeOperation

-- ============================================================================
-- * TOP-LEVEL COMPILER
-- ============================================================================

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
  , appIRStates     = nub $ collectAllStates validators
  }

-- | Compiles an 'SStateList' into a list of 'StateInfoIR'.
compileStateList :: SStateList states -> [StateInfoIR]
compileStateList SSNil = []
compileStateList (SSCons spec@(SStateSpec fields _ _ mappable) rest) =
  StateInfoIR
    { stateInfoName      = extractStateName spec
    , stateInfoDatumName = extractDatumName spec
    , stateInfoTokenName = extractTokenName spec
    , stateInfoPolicy = compilePolicySource (extractPolicySource spec)
    , stateInfoMappable = isMappable mappable
    , stateInfoFields = compileDatumFields fields
    } : compileStateList rest

-- ============================================================================
-- * STATE & POLICY COMPILATION
-- ============================================================================

-- | (Internal) Collects all 'StateInfoIR' from all validators in the application.
collectAllStates :: SValidatorList vs -> [StateInfoIR]
collectAllStates SVNil = []
collectAllStates (SVCons (SValidator _ managedStates _ _) rest) =
  compileStateList managedStates ++ collectAllStates rest

-- | (Internal) Compiles a generic policy source singleton into its IR representation.
compilePolicySource :: SomePolicySource -> PolicySourceIR
compilePolicySource (SomePolicySource SOwnPolicy) = OwnPolicyIR
compilePolicySource (SomePolicySource (SExternalPolicy sym)) = ExternalPolicyIR (pack $ symbolVal sym)

-- | (Internal) Compiles the singleton list of datum fields into their IR representation.
-- Uses 'Typeable' to extract the Haskell type as text for code generation.
compileDatumFields :: SDatumFieldList fields -> [DatumFieldIR]
compileDatumFields SDFNil = []
compileDatumFields (SDFCons (SDatumField nameProxy (_ :: Proxy t)) rest) =
  let
    fieldName = pack $ symbolVal nameProxy
    -- Show the TypeRep to get the Haskell type as a string (e.g., "Integer", "Maybe PubKeyHash")
    fieldType = pack $ show (typeRep (Proxy @t))
  in
    DatumFieldIR fieldName fieldType : compileDatumFields rest

-- | (Internal) Converts an 'SMappability' singleton into a generic 'Bool' for the IR.
isMappable :: SMappability m -> Bool
isMappable SMappable    = True
isMappable SNotMappable = False

-- ============================================================================
-- * VALIDATOR & ACTION COMPILATION
-- ============================================================================

-- | (Internal) Compiles an 'SValidatorList' into a list of 'ValidatorIR's.
compileValidators :: SAppSpec app -> SValidatorList vs -> SActionTransitionList app ts -> SParamDerivationList ds -> [ValidatorIR]
compileValidators spec validators transitions derivations = go validators
 where
  -- Helper function to recurse through the validator list
  go :: SValidatorList vs_go -> [ValidatorIR]
  go SVNil = []
  go (SVCons v rest) =
   let validator = compileValidator spec v transitions derivations -- Compile head
   in validator : go rest -- Compile tail

-- | (Internal) Compiles a single 'SValidator' into a 'ValidatorIR'.
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

-- | (Internal) Scans the application's 'ActionTransitions' to find and compile all 'ActionIR's
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

-- | (Internal) Determines if an 'SActionSpec' is relevant to a given 'SValidator'.
-- Checks if any step operates on a managed state or if any constraint targets the validator.
actionAffectsValidator :: forall v spec. ValidatorSpec v => SValidator v -> SActionSpec spec -> Bool
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

-- | (Internal) Checks if a single 'SActionStep' affects the validator.
stepAffectsValidator :: SValidator v -> SActionStep s -> Bool
stepAffectsValidator v step = case step of
  SOp op -> opAffectsValidator v op
  SLet _ op -> opAffectsValidator v op
  SMap op _ _ -> opAffectsValidator v op

-- | (Internal) Checks if an 'SOperation' affects the validator. True if the operation
-- (Create, Update, Delete) targets a state managed by the validator.
opAffectsValidator :: SValidator v -> SOperation op -> Bool
opAffectsValidator (SValidator _ managedStates _ _) op =
  case op of
    SReference _ _ -> False
    _ -> stateInList (getOpStateType op) managedStates

-- | (Internal) Checks if an 'SConstraint' directly affects or targets the validator.
constraintAffectsValidator :: forall v c. ValidatorSpec v => SValidator v -> SConstraint c -> Bool
constraintAffectsValidator v (SMustSpendValidatorParam vNameProxy _) =
  pack (symbolVal vNameProxy) == getValidatorNameFromSingleton v
constraintAffectsValidator v (SMustWithdrawFromAggregateState st _ _) =
  validatorManagesState v (SomeStateType st)
constraintAffectsValidator _ _ = False

-- ============================================================================
-- * OPERATION & VALUE COMPILATION
-- ============================================================================

-- | (Internal) Compiles an 'SActionSpec' into an 'ActionIR'.
compileAction :: SAppSpec app
              -> SParamList vParams -- ^ Parameters of the current validator being compiled.
              -> SParamDerivationList ds -- ^ All app derivation rules.
              -> SActionSpec spec
              -> ActionIR
compileAction appSpec vParams derivations actionSpec@(SActionSpec nameProxy steps constraints _) = ActionIR
  { actionIRName        = pack $ symbolVal nameProxy
  , actionIROperations  = compileOperations steps
  , actionIRConstraints = compileConstraints constraints ++ generateInstanceChecks appSpec vParams derivations actionSpec
  }

-- | (Internal) Compiles an 'SActionStepList' into a list of 'OperationIR'.
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

-- | (Internal) Compiles a single 'SOperation' (excluding 'SReference' outside 'SLet') into an 'OperationIR'.
compileOperation :: SOperation op -> OperationIR
compileOperation (SCreate st fields _) = CreateOpIR
  { opIRStateName = extractStateName st
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

-- | (Internal) Compiles an 'SFieldSpecList' into a list of '(FieldName, FieldValueIR)' tuples.
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

-- | (Internal) Compiles an 'STypedValue' singleton into a 'FieldValueIR' value.
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

-- | (Internal) Compiles the inner operation of a 'Map' step into a 'BatchOperationIR'.
compileBatchOperation :: SOperation op -> BatchOperationIR
compileBatchOperation (SCreate st fields _) = BatchCreateIR
  { batchOpIRStateName = extractStateName st
  , batchOpIRFields    = compileFields fields
  }
compileBatchOperation (SDelete ref _) = BatchDeleteIR
  { batchOpIRStateName = extractStateFromRef ref
  }
compileBatchOperation _ = error "Only Create and Delete operations are supported in a Map"

-- | (Internal) Compiles an 'SCollectionConstraintList' into a list of 'CollectionConstraintIR'.
compileCollectionConstraints :: SCollectionConstraintList cs -> [CollectionConstraintIR]
compileCollectionConstraints SCCNil = []
compileCollectionConstraints (SCCCons c rest) = compileCollectionConstraint c : compileCollectionConstraints rest

-- | (Internal) Compiles a single 'SCollectionConstraint' into a 'CollectionConstraintIR'.
compileCollectionConstraint :: SCollectionConstraint c -> CollectionConstraintIR
compileCollectionConstraint (SMustHaveUniqueField fieldProxy) = MustHaveUniqueFieldIR (pack $ symbolVal fieldProxy)

-- ============================================================================
-- * Constraint & Instance Check Compilation
-- ============================================================================

-- | (Internal) Compiles an 'SConstraintList' into a list of 'ConstraintIR', filtering out constraints
-- handled implicitly or not relevidatorParam').
compileConstraints :: SConstraintList cs -> [ConstraintIR]
compileConstraints SCLNil = []
compileConstraints (SCLCons c rest) =
  case compileConstraint c of
    Just ir -> ir : compileConstraints rest -- Add compiled constraint if relevant
    Nothing -> compileConstraints rest -- Skip irrelevant constraint

-- | (Internal) Compiles a single 'SConstraint' into a 'Maybe ConstraintIR'.
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
  Just $ MustAddToAggregateStateIR (extractStateName st) (compileValue val)
compileConstraint (SMustWithdrawFromAggregateState st val addr) =
  -- Compiles directly, includes state name and compiled value/address
  Just $ MustWithdrawFromAggregateStateIR (extractStateName st) (compileValue val) (compileValue addr)
compileConstraint (SMustSpendValidatorParam _ _) = Nothing
compileConstraint (SMustNotExist _) = Nothing
compileConstraint (SPreserveStateField _ _) = Nothing
compileConstraint (SRequireStateValue {}) = Nothing
compileConstraint (SMustExist _) = Nothing
compileConstraint (SExactlyN _ _) = Nothing
compileConstraint (SAtLeastN _ _) = Nothing
compileConstraint (SAtMostN _ _) = Nothing
compileConstraint (SMustSpendParam _) = Nothing
compileConstraint (SMustBeSignedByValidatorParam _ _) = Nothing

-- | (Internal) Generates implicit 'MustCheckInstance' constraints ('InstanceCheckIR') based on
-- 'Reference' operations within an action and the parameter derivation rules of the application.
-- Ensures that referenced states belong to the correct application instance, especially when
-- validators derive parameters (like addresses) from each other.
generateInstanceChecks :: SAppSpec app
                       -> SParamList vParams -- ^ Parameters of the validator being compiled.
                       -> SParamDerivationList ds -- ^ All app derivation rules.
                       -> SActionSpec spec -- ^ The action being compiled.
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

-- | (Internal) Checks a single step (specifically 'Reference' ops) to see if an instance check is needed.
checkStepForInstanceConstraint :: SAppSpec app
                               -> SParamList vParams -- ^ Current validator's params
                               -> SParamDerivationList ds -- ^ All derivations
                               -> (Maybe Text, SomeOperation) -- ^ The step (label, operation)
                               -> Maybe ConstraintIR -- ^ 'Just MustCheckInstance' if needed, else 'Nothing'
checkStepForInstanceConstraint appSpec vParams derivations (_, SomeOperation op) = case op of
  SReference ref _constraints -> checkReference appSpec ref vParams derivations
  _ -> Nothing

-- | (Internal) Analyzes a 'Reference' operation ('SStateRef') to determine if an 'InstanceCheckIR' is required.
-- An instance check is needed if:
-- 1. The *current* validator has an 'Address' parameter derived ('DeriveParam') from another validator ('ValidatorAddress').
-- 2. The state being referenced ('referencedStateRef') is managed by that *same source validator*.
-- This ensures the referenced state belongs to the same instance anchor as the current validator.
checkReference :: forall (s :: Type) app (ref :: TypedStateRef s) vParams ds.
                  (StateSpec s)
               => SAppSpec app
               -> SStateRef s ref -- ^ The state reference being checked.
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

        let sStateSpec = extractStateTypeFromRef' referencedStateRef
        let referencedDatumName = pack $ symbolVal (Proxy @(DatumName s))

        case findValidatorManagingState appSpec (SomeStateType sStateSpec) of
            Left _ -> Nothing
            Right (SomeValidator managingValidator) -> do
                let managingValidatorName = getValidatorNameFromSingleton managingValidator
                if managingValidatorName == sourceValidatorName then
                    Just $ MustCheckInstance $ AddressMatchesParamIR
                      { amprReferenceState = referencedDatumName -- Use the datum name here
                      , amprParamName      = addressParamName
                      }
                else
                    Nothing
    _ -> Nothing

-- | (Internal) Finds the name ('Text') of the first parameter of type 'Address' in an 'SParamList'.
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

-- ============================================================================
-- * Helper Functions
-- ============================================================================

-- | Helper to get the 'SomeStateType' existential wrapper from an 'SOperation'.
getOpStateType :: SOperation op -> SomeStateType
getOpStateType (SCreate stype _ _) = SomeStateType stype
getOpStateType (SUpdate ref _ _)   = SomeStateType (extractStateTypeFromRef' ref)
getOpStateType (SDelete ref _)     = SomeStateType (extractStateTypeFromRef' ref)
getOpStateType (SReference ref _)  = SomeStateType (extractStateTypeFromRef' ref)