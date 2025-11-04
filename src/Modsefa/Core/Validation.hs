{-|
Module      : Modsefa.Core.Validation
Description : Compile-time validation rules for Modsefa action specifications.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC (requires TypeFamilies, GADTs, UndecidableInstances, etc.)

This module defines the core type-level validation logic for Modsefa 'TypedActionSpec's.
The primary export is the 'IsActionSpecValid' type class, which acts as a constraint.
If an action specification @spec@ satisfies @'IsActionSpecValid' spec@, it indicates
that the specification adheres to the rules defined herein (e.g., correct field types,
consistent state management, valid constraints, instance consistency).

This validation is primarily enforced through the 'Modsefa.Core.Actions.mkTypedAction'
smart constructor.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Modsefa.Core.Validation 
  ( IsActionSpecValid
  ) where

import Data.Kind (Constraint, Type)
import Data.Type.Bool (If)
import Data.Type.Equality (type (==))
import GHC.Generics (C1, D1, Generic (..), Meta(..), Rep, V1, (:+:))
import GHC.Records (HasField)
import GHC.TypeLits 
  ( CmpSymbol, ErrorMessage(ShowType, Text, (:<>:)), KnownSymbol, Symbol, TypeError
  )

import PlutusLedgerApi.V3 (POSIXTime, TxOutRef)

import Modsefa.Core.Foundation
  ( ActionSpecConstraints, ActionSpecSteps, ActionStep(..)
  , AppSpec(AppInstanceParameters, ParameterDerivations, Validators)
  , DerivationSource(ValidatorAddress, ValidatorHash), FieldSpec(..)
  , LookupValidatorParamType, ParamDerivation(..), StateType(..), TypedActionSpec
  , TypedConstraint(..), TypedOperation(..), TypedStateRef(..), TypedValue(..)
  , ValidatorDef(..), ValidatorExistsInApp, ValidatorParamFieldExists
  , ValidatorSpec(ManagedStates, Params, ValidatorAppName)
  )


-- ============================================================================
-- 1. Core Validation Helpers
-- ============================================================================

-- | Helper type class used with 'ValidateFieldSpecs' to check for a field's existence
-- using 'HasField' without triggering GHC's constraint wildcard errors.
class Exists (s :: Symbol) r
instance (HasField s r t) => Exists s r

-- | Helper to extract the underlying Haskell record 'Type' from a 'StateType'.
type family GetRecord (st :: StateType) :: Type where
  GetRecord ('ST _ record) = record

-- | Helper to extract the 'StateType' from a 'TypedStateRef'.
type family GetStateTypeFromRef (ref :: TypedStateRef st) :: StateType where
  GetStateTypeFromRef (ref :: TypedStateRef st) = st

-- ============================================================================
-- 2. The Validation Layer Entry Point
-- ============================================================================

-- | Top-level validation constraint for a 'TypedActionSpec'.
-- An action specification @spec@ satisfies @'IsActionSpecValid' spec@ if and only if
-- its steps ('ValidateActionSteps'), constraints ('ValidateConstraints'), and
-- instance consistency ('VerifyInstanceConsistency') are all valid according
-- to the type-level rules defined in this module.
-- This constraint is checked by 'Modsefa.Core.Actions.mkTypedAction'.
class
  ( AppSpec app -- Requires the application context
  , ValidateActionSteps (ActionSpecSteps spec) app -- Validates the sequence of operations
  , ValidateConstraints (ActionSpecConstraints spec) app -- Validates the transaction-level constraints
  , VerifyInstanceConsistency (ActionSpecSteps spec) (ActionSpecConstraints spec) app -- Checks instance consistency
  ) => IsActionSpecValid (spec :: TypedActionSpec app)

-- | The sole instance of 'IsActionSpecValid'. It simply requires all the necessary
-- sub-validations (defined as superclasses) to hold. GHC's instance resolution
-- mechanism triggers the evaluation of these validation type families
instance
  ( AppSpec app
  , ValidateActionSteps (ActionSpecSteps spec) app
  , ValidateConstraints (ActionSpecConstraints spec) app
  , VerifyInstanceConsistency (ActionSpecSteps spec) (ActionSpecConstraints spec) app
  ) => IsActionSpecValid (spec :: TypedActionSpec app)

-- ============================================================================
-- 3. Type-Level Instance Consistency Validation
-- ============================================================================
-- This section defines type families that trace validator dependencies
-- through parameters and derivations to ensure all states accessed within
-- a single action belong to the same logical application instance.

-- | Entry point: Ensures all states referenced in an action trace back to a common root anchor ('AppInstanceParameters').
-- Gathers all states, finds their roots, and checks if all non-empty roots are identical.
type family VerifyInstanceConsistency (steps :: [ActionStep]) (constraints :: [TypedConstraint]) (app :: Type) :: Constraint where
  VerifyInstanceConsistency steps constraints app =
    AllRootsEqual (GetAllInstanceRoots (CollectStates steps constraints) app)

-- | Collects all unique 'StateType's mentioned in an action's steps and constraints.
type family CollectStates (steps :: [ActionStep]) (constraints :: [TypedConstraint]) :: [StateType] where
  CollectStates steps constraints = RemoveDuplicates (CollectStatesFromSteps steps ++ CollectStatesFromConstraints constraints)

-- | Applies 'GetInstanceRoot' to each state in a list.
type family GetAllInstanceRoots (states :: [StateType]) (app :: Type) :: [[(Symbol, Symbol)]] where
  GetAllInstanceRoots '[] _ = '[]
  GetAllInstanceRoots (st ': sts) app = GetInstanceRoot st app ': GetAllInstanceRoots sts app

-- | Finds the root 'AppInstanceParameters' anchor(s) for a single 'StateType' by tracing its managing validator.
type family GetInstanceRoot (st :: StateType) (app :: Type) :: [(Symbol, Symbol)] where
  GetInstanceRoot st app =
    TraceValidator (FindManagingValidator st (Validators app)) app

-- | Traces a validator's dependencies to find its root anchor(s).
-- If the validator is directly parameterized by an 'AppInstanceParameters', returns that parameter.
-- Otherwise, attempts to trace through its derived parameters ('TraceDerivedValidator').
type family TraceValidator (vDef :: ValidatorDef) (app :: Type) :: [(Symbol, Symbol)] where
  TraceValidator ('Validator v) app =
    If (IsRootValidator v (AppInstanceParameters app))
       (GetRootParam v (AppInstanceParameters app)) -- Base case: Found a root parameterization
       (TraceDerivedValidator v app) -- Recursive case: Follow derivations

-- | Recursively traces the dependencies of a derived validator (one not directly parameterized by 'AppInstanceParameters').
type family TraceDerivedValidator (v :: Type) (app :: Type) :: [(Symbol, Symbol)] where
  TraceDerivedValidator v app =
    FollowDerivations (Params v) (ParameterDerivations app) (Validators app) app

-- | Follows the derivation chain for a validator's parameter list.
-- If a parameter is derived from another validator ('ValidatorAddress', 'ValidatorHash'),
-- recursively calls 'TraceValidator' on the source validator.
-- Returns '[]' if no derivable parameter path leads to a root (indicating a dynamic validator like CustomerValidator, which is valid).
type family FollowDerivations (vParams :: [(Symbol, Type)]) (derivations :: [ParamDerivation]) (allValidators :: [ValidatorDef]) (app :: Type) :: [(Symbol, Symbol)] where
  FollowDerivations '[] _ _ _ = '[] -- Base case: No more parameters to check
  FollowDerivations ('(pName, _) ': ps) derivations allValidators app =
      -- Attempt to follow the derivation for the current parameter,
      -- or continue with the rest if no rule applies to this one.
      -- Note: This assumes the *first* matching derivation determines the path.
      FindAndFollowDerivation pName derivations allValidators app

-- | Finds a specific 'ParamDerivation' rule by parameter name and continues the trace.
type family FindAndFollowDerivation (pName :: Symbol) (derivations :: [ParamDerivation]) (allValidators :: [ValidatorDef]) (app :: Type) :: [(Symbol, Symbol)] where
  -- Found a derivation rule; trace the source validator
  FindAndFollowDerivation pName ('DeriveParam pName ('ValidatorAddress vName) ': _) allValidators app =
    TraceValidator (FindValidatorByName vName allValidators) app
  FindAndFollowDerivation pName ('DeriveParam pName ('ValidatorHash vName) ': _) allValidators app =
    TraceValidator (FindValidatorByName vName allValidators) app
  -- Rule doesn't match this parameter; try the next rule in the list
  FindAndFollowDerivation pName (_ ': ds) allValidators app = FindAndFollowDerivation pName ds allValidators app
  -- No derivation rule found for this parameter name; stop tracing this path
  FindAndFollowDerivation pName '[] _ _ = '[]

-- ============================================================================
-- 4. The Validation Logic
-- ============================================================================
-- This section contains type families that perform detailed checks on operations,
-- field specifications, constraints, etc., ensuring they are well-formed and type-correct.

-- | (Internal) Recursively validates a list of 'ActionStep's against an 'AppSpec'.
type family ValidateActionSteps (steps :: [ActionStep]) (app :: Type) :: Constraint where
  ValidateActionSteps '[] _ = ()
  ValidateActionSteps (step ': rest) app = (ValidateActionStep step app, ValidateActionSteps rest app)

-- | (Internal) Validates a single 'ActionStep' by dispatching to 'ValidateOperation'.
type family ValidateActionStep (step :: ActionStep) (app :: Type) :: Constraint where
  ValidateActionStep ('Op op) app = ValidateOperation op app
  ValidateActionStep ('Let _ op) app = ValidateOperation op app
  ValidateActionStep ('Map op param _) app = ValidateOperation op app -- TODO: Validate 'param' is a mappable list type

-- | (Internal/Placeholder) Recursively validates a list of 'TypedOperation's.
type family ValidateOperations (ops :: [TypedOperation]) (app :: Type) :: Constraint where
  ValidateOperations '[] _ = ()
  ValidateOperations (op ': rest) app = (ValidateOperation op app, ValidateOperations rest app) -- Assumes ValidateOperation exists

-- | (Internal) Validates a single 'TypedOperation'. Checks that the state is managed by some validator,
-- validates field specifications ('ValidateFieldSpecs'), and validates state references ('ValidateStateRef').
type family ValidateOperation (op :: TypedOperation) (app :: Type) :: Constraint where
  ValidateOperation ('Create @('ST name record) specs constraints) app =
    ( CheckValidatorManagesState ('ST name record) app
    , ValidateFieldSpecs record specs
    -- TODO: Validate operation-level constraints if needed
    )
  ValidateOperation ('Update (ref :: TypedStateRef ('ST name record)) specs constraints) app =
    ( CheckValidatorManagesState ('ST name record) app
    , ValidateFieldSpecs record specs
    , ValidateStateRef ref
    -- TODO: Validate operation-level constraints if needed
    )
  ValidateOperation ('Delete (ref :: TypedStateRef st) constraints) app =
    ( CheckValidatorManagesState (GetStateTypeFromRef ref) app
    , ValidateStateRef ref
    -- TODO: Validate operation-level constraints if needed
    )
  ValidateOperation ('Reference (ref :: TypedStateRef st) constraints) app =
    ( CheckValidatorManagesState (GetStateTypeFromRef ref) app
    , ValidateStateRef ref
    -- TODO: Validate operation-level constraints if needed
    )

-- | (Internal) Type class for validating a list of 'FieldSpec's against a target record 'Type'.
-- Uses functional dependencies or type families internally to check field existence and type compatibility.
class ValidateFieldSpecs (record :: Type) (specs :: [FieldSpec])
instance ValidateFieldSpecs record '[] -- Base case: Empty list is valid.

-- Instances for different FieldSpec constructors ('SetTo', 'Preserve') check field existence and value type compatibility.
instance (Exists field record, ValidateFieldSpecs record rest)
  => ValidateFieldSpecs record ('SetTo field ('ParamValue s) ': rest)

instance (HasField field record fieldType, fieldType ~ t, CheckEnumValue t s, ValidateFieldSpecs record rest)
  => ValidateFieldSpecs record ('SetTo field ('EnumValue t s) ': rest)

instance (Exists field record, ValidateFieldSpecs record rest)
  => ValidateFieldSpecs record ('Preserve field ': rest)

instance ( HasField field record fieldType
         , ValidateFieldSpecs record rest
         ) => ValidateFieldSpecs record ('SetTo field ('StateFieldValue label fname) ': rest)

instance ( HasField field record POSIXTime
         , ValidateFieldSpecs record rest
         ) => ValidateFieldSpecs record ('SetTo field 'CurrentTime ': rest)

instance ( HasField field record fieldType
         , ValidateValue v1 fieldType
         , ValidateValue v2 fieldType
         , ValidateFieldSpecs record rest
         ) => ValidateFieldSpecs record ('SetTo field ('AddValue v1 v2) ': rest)

instance ( HasField field record fieldType
         , ValidateValue v1 fieldType
         , ValidateValue v2 fieldType
         , ValidateFieldSpecs record rest
         ) => ValidateFieldSpecs record ('SetTo field ('SubtractValue v1 v2) ': rest)

instance ( HasField field record fieldType
         , ValidateValue v1 fieldType
         , ValidateValue v2 fieldType
         , ValidateFieldSpecs record rest
         ) => ValidateFieldSpecs record ('SetTo field ('MultiplyValue v1 v2) ': rest)

instance ( HasField field record fieldType
         , ValidateValue v1 fieldType
         , ValidateValue v2 fieldType
         , ValidateFieldSpecs record rest
         ) => ValidateFieldSpecs record ('SetTo field ('DivideValue v1 v2) ': rest)

-- | (Internal/Placeholder) Recursively validates a list of 'TypedConstraint's.
type family ValidateConstraints (constraints :: [TypedConstraint]) (app :: Type) :: Constraint where
  ValidateConstraints '[] _ = ()
  ValidateConstraints (c ': cs) app = (ValidateConstraint c app, ValidateConstraints cs app)

-- | (Internal) Validates the structure of a 'TypedStateRef' itself.
-- Currently accepts all defined constructors.
type family ValidateStateRef (ref :: TypedStateRef st) :: Constraint where
  ValidateStateRef 'TypedTheOnlyInstance = ()
  ValidateStateRef ('TypedUniqueWhere @_ _) = ()
  ValidateStateRef ('TypedByLabel l) = ()
  ValidateStateRef 'TypedAny = ()
  ValidateStateRef ('TypedAnyWhere _) = ()

-- | (Internal) Validates a single 'TypedConstraint'. Checks referenced validators/parameters exist,
-- referenced state fields exist, type compatibility, etc.
type family ValidateConstraint (constraint :: TypedConstraint) (app :: Type) :: Constraint where
  
  -- MustBeSignedByState: Check state is managed, field exists in state record, ref is valid.
  ValidateConstraint ('MustBeSignedByState (ref :: TypedStateRef st) field) app =
    ( CheckValidatorManagesState (GetStateTypeFromRef ref) app
    , Exists field (GetRecord (GetStateTypeFromRef ref))
    , ValidateStateRef ref
    )

  -- MustNotExist/MustExist/Counting: Just validate the reference for now.
  ValidateConstraint ('ExactlyN n (ref :: TypedStateRef st)) app = ValidateStateRef ref
  ValidateConstraint ('MustExist (ref :: TypedStateRef st)) app = ValidateStateRef ref
  ValidateConstraint ('MustNotExist (ref :: TypedStateRef st)) app = ValidateStateRef ref
  ValidateConstraint ('AtLeastN n (ref :: TypedStateRef st)) app = ValidateStateRef ref
  ValidateConstraint ('AtMostN n (ref :: TypedStateRef st)) app = ValidateStateRef ref

  -- Parameter Constraints: Check validator/param existence and potentially type.
  ValidateConstraint ('MustSpendValidatorParam vName param) app =
    ( ValidatorExistsInApp vName app
    , ValidatorParamFieldExists vName param app
    , ValidatorParamIsType vName param TxOutRef app
    , KnownSymbol vName
    , KnownSymbol param
    )
  ValidateConstraint ('MustSpendActionParam param) app =
    ( KnownSymbol param
    -- TODO: Check action actually has this parameter and it's a TxOutRef
    )
  ValidateConstraint ('MustBeSignedByParam param) app =
    ( KnownSymbol param
    -- TODO: Check action actually has this parameter and it's a PubKeyHash
    )
  ValidateConstraint ('MustBeSignedByValidatorParam vName param) app =
    ( ValidatorExistsInApp vName app
    , ValidatorParamFieldExists vName param app
    , KnownSymbol vName
    , KnownSymbol param
    )

  -- Aggregate State Constraints: Check state is managed, validate value type.
  ValidateConstraint ('MustAddToAggregateState st val) app =
    ( CheckValidatorManagesState st app -- Check state is managed
    -- TODO: Validate 'val' resolves to the correct asset type for 'st'
    )
  ValidateConstraint ('MustWithdrawFromAggregateState st val addr) app =
    ( CheckValidatorManagesState st app -- Check state is managed
    -- TODO: Validate 'val' resolves to the correct asset type for 'st'
    -- TODO: Validate 'addr' resolves to Address
    )

  ValidateConstraint _ _ = ()

-- | (Internal) Type class check: Ensures a 'StateType' @st@ is listed in the 'ManagedStates'
-- of at least one validator within the application @app@.
-- Used by 'ValidateOperation' for Create/Update/Delete.
class CheckValidatorManagesState (st :: StateType) (app :: Type)
instance (AppSpec app, CheckValidatorManagesState' st (Validators app)) => CheckValidatorManagesState st app

-- | Helper class for 'CheckValidatorManagesState': Recursively searches the validator list.
class CheckValidatorManagesState' (st :: StateType) (vDefs :: [ValidatorDef])
instance (TypeError ('Text "State " ':<>: 'ShowType st ':<>: 'Text " not managed by any validator in AppSpec."))
  => CheckValidatorManagesState' st '[]
instance
  ( ValidatorSpec v
  , If (IsStateInList st (ManagedStates v))
      (() :: Constraint)
      (CheckValidatorManagesState' st rest)
  ) => CheckValidatorManagesState' st ('Validator v ': rest)

-- ============================================================================
-- 5. Lower-Level Generic Helpers
-- ============================================================================
-- These are supporting type families used by the main validation logic above.

-- | Type-level check if a 'StateType' is present in a list of 'StateType's.
type family IsStateInList (st :: StateType) (states :: [StateType]) :: Bool where
  IsStateInList st (st ': _) = 'True
  IsStateInList st (_ ': rest) = IsStateInList st rest
  IsStateInList st '[] = 'False

-- | (Internal) Constraint check: Ensures a 'Symbol' @value@ corresponds to a valid
-- constructor name for a 'Generic' enumeration type @enum@.
type family CheckEnumValue (enum :: Type) (value :: Symbol) :: Constraint where
  CheckEnumValue enum value = (Generic enum, ContainsConstructor (GetConstructors (Rep enum)) value)

-- Helper type families for CheckEnumValue (GetConstructors, GetConstructorsSum, ContainsConstructor, Append)
type family GetConstructors (rep :: Type -> Type) :: [Meta] where
  GetConstructors (D1 ('MetaData _ _ _ _) f) = GetConstructorsSum f

type family GetConstructorsSum (rep :: Type -> Type) :: [Meta] where
  GetConstructorsSum (C1 c f) = '[c]
  GetConstructorsSum (l :+: r) = Append (GetConstructorsSum l) (GetConstructorsSum r)
  GetConstructorsSum V1 = '[]

type family ContainsConstructor (ctors :: [Meta]) (value :: Symbol) :: Constraint where
  ContainsConstructor '[] value = TypeError ('Text "Value '" ':<>: 'ShowType value ':<>: 'Text "' is not a valid constructor.")
  ContainsConstructor ('MetaCons name _ _ ': rest) value = If (CmpSymbol name value == 'EQ) (() :: Constraint) (ContainsConstructor rest value)

type family Append (l :: [k]) (r :: [k]) :: [k] where
  Append '[] r = r
  Append (h ': t) r = h ': Append t r

-- | (Internal) Validates a 'TypedValue' against an expected 'Type'. Used within 'ValidateFieldSpecs'.
type family ValidateValue (value :: TypedValue) (expectedFieldType :: Type) :: Constraint
type instance ValidateValue ('EnumValue t s) expectedFieldType = (expectedFieldType ~ t, CheckEnumValue t s)
type instance ValidateValue ('ParamValue s) _ = ()
type instance ValidateValue ('StateFieldValue _ _) _ = ()
type instance ValidateValue 'CurrentTime POSIXTime = ()
type instance ValidateValue ('IntValue n) Integer = ()
type instance ValidateValue ('AddValue v1 v2) expectedFieldType =
  (ValidateValue v1 expectedFieldType, ValidateValue v2 expectedFieldType)
type instance ValidateValue ('SubtractValue v1 v2) expectedFieldType = (ValidateValue v1 expectedFieldType, ValidateValue v2 expectedFieldType)
type instance ValidateValue ('MultiplyValue v1 v2) expectedFieldType = (ValidateValue v1 expectedFieldType, ValidateValue v2 expectedFieldType)
type instance ValidateValue ('DivideValue v1 v2) expectedFieldType = (ValidateValue v1 expectedFieldType, ValidateValue v2 expectedFieldType)

-- | (Internal) Checks if a validator parameter ('vName', 'param') in an application @app@ has the 'expectedType'.
type family ValidatorParamIsType (vName :: Symbol) (param :: Symbol) (expectedType :: Type) (app :: Type) :: Constraint where
  ValidatorParamIsType vName param expectedType app = 
    (LookupValidatorParamType vName param app ~ expectedType)

-- Helper type families for collecting states during instance consistency checks
type family CollectStatesFromSteps (steps :: [ActionStep]) :: [StateType] where
  CollectStatesFromSteps '[] = '[]
  CollectStatesFromSteps ('Op op ': ss) = GetOpStates op ++ CollectStatesFromSteps ss
  CollectStatesFromSteps ('Let _ op ': ss) = GetOpStates op ++ CollectStatesFromSteps ss
  CollectStatesFromSteps ('Map op _ _ ': ss) = GetOpStates op ++ CollectStatesFromSteps ss

type family CollectStatesFromConstraints (constraints :: [TypedConstraint]) :: [StateType] where
  CollectStatesFromConstraints '[] = '[]
  CollectStatesFromConstraints (c ': cs) = GetConstraintStates c ++ CollectStatesFromConstraints cs

type family GetOpStates (op :: TypedOperation) :: [StateType] where
  GetOpStates ('Create @('ST name record) _ _) = '[ 'ST name record ]
  GetOpStates ('Update ref _ _) = '[GetStateTypeFromRef ref]
  GetOpStates ('Delete ref _) = '[GetStateTypeFromRef ref]
  GetOpStates ('Reference ref _) = '[GetStateTypeFromRef ref]

type family GetConstraintStates (c :: TypedConstraint) :: [StateType] where
  GetConstraintStates ('MustBeSignedByState ref _) = '[GetStateTypeFromRef ref]
  GetConstraintStates ('MustNotExist ref) = '[GetStateTypeFromRef ref]
  GetConstraintStates ('MustExist ref) = '[GetStateTypeFromRef ref]
  GetConstraintStates ('ExactlyN _ ref) = '[GetStateTypeFromRef ref]
  GetConstraintStates ('AtMostN _ ref) = '[GetStateTypeFromRef ref]
  GetConstraintStates ('AtLeastN _ ref) = '[GetStateTypeFromRef ref]
  GetConstraintStates ('PreserveStateField ref _) = '[GetStateTypeFromRef ref]
  GetConstraintStates ('RequireStateValue ref _ _) = '[GetStateTypeFromRef ref]
  GetConstraintStates _ = '[]

-- | (Internal) Helper for instance consistency: Finds the 'ValidatorDef' managing a given 'StateType'.
type family FindManagingValidator (st :: StateType) (vs :: [ValidatorDef]) :: ValidatorDef where
  FindManagingValidator st ('Validator v ': vs) =
    If (IsStateInList st (ManagedStates v))
       ('Validator v)
       (FindManagingValidator st vs)
  FindManagingValidator st '[] = TypeError ('Text "Internal validation error: Could not find validator for state " ':<>: 'ShowType st)

-- List utility type families used for instance consistency checks
type family (++) (l :: [k]) (r :: [k]) :: [k] where
  '[] ++ r = r
  (h ': t) ++ r = h ': (t ++ r)

type family RemoveDuplicates (l :: [k]) :: [k] where
  RemoveDuplicates '[] = '[]
  RemoveDuplicates (x ': xs) = x ': RemoveDuplicates (Filter x xs)

type family Filter (x :: k) (l :: [k]) :: [k] where
  Filter x '[] = '[]
  Filter x (x ': xs) = Filter x xs
  Filter x (y ': xs) = y ': Filter x xs

-- Helper type families for instance consistency check: IsRootValidator, IsRootParamList, IsInAppParams, GetRootParam, GetRootParamList
type family IsRootValidator (v :: Type) (appParams :: [(Symbol, Symbol)]) :: Bool where
  IsRootValidator v appParams = IsRootParamList (Params v) (ValidatorAppName v) appParams

type family IsRootParamList (vParams :: [(Symbol, Type)]) (vName :: Symbol) (appParams :: [(Symbol, Symbol)]) :: Bool where
  IsRootParamList '[] _ _ = 'False
  IsRootParamList ('(pName, _) ': ps) vName appParams = IsInAppParams vName pName appParams || IsRootParamList ps vName appParams

type family IsInAppParams (vName :: Symbol) (pName :: Symbol) (appParams :: [(Symbol, Symbol)]) :: Bool where
  IsInAppParams vName pName ('(vName, pName) ': _) = 'True
  IsInAppParams vName pName (_ ': xs) = IsInAppParams vName pName xs
  IsInAppParams _ _ '[] = 'False

type family GetRootParam (v :: Type) (appParams :: [(Symbol, Symbol)]) :: [(Symbol, Symbol)] where
  GetRootParam v appParams = Concat (GetRootParamList (Params v) (ValidatorAppName v) appParams)

type family GetRootParamList (vParams :: [(Symbol, Type)]) (vName :: Symbol) (appParams :: [(Symbol, Symbol)]) :: [[(Symbol, Symbol)]] where
  GetRootParamList '[] _ _ = '[]
  GetRootParamList ('(pName, _) ': ps) vName appParams =
    If (IsInAppParams vName pName appParams)
       '[ '[ '(vName, pName) ] ]
       (GetRootParamList ps vName appParams)

-- | (Internal) Helper for instance consistency: Finds a 'ValidatorDef' by its 'ValidatorAppName'.
type family FindValidatorByName (vName :: Symbol) (validators :: [ValidatorDef]) :: ValidatorDef where
  FindValidatorByName vName ('Validator v ': vs) =
    If (ValidatorAppName v == vName)
       ('Validator v)
       (FindValidatorByName vName vs)
  FindValidatorByName vName '[] = TypeError ('Text "Instance trace failed: Could not find validator named '" ':<>: 'ShowType vName ':<>: 'Text "'.")
  
-- Helper type families for instance consistency check: AllRootsEqual, AllRootsEqual', FilterOutEmpty
type family AllRootsEqual (roots :: [[(Symbol, Symbol)]]) :: Constraint where
  AllRootsEqual roots = AllRootsEqual' (FilterOutEmpty roots)

type family AllRootsEqual' (nonEmptyRoots :: [[(Symbol, Symbol)]]) :: Constraint where
  AllRootsEqual' '[] = TypeError ('Text "Instance Consistency Check Failed: Could not trace any state in the action to a root AppInstanceParameter to act as an anchor.")
  AllRootsEqual' '[_] = ()
  AllRootsEqual' (r1 ': r2 ': rs) = (r1 ~ r2, AllRootsEqual' (r2 ': rs))

type family FilterOutEmpty (lists :: [[k]]) :: [[k]] where
  FilterOutEmpty '[] = '[]
  FilterOutEmpty ('[] ': ls) = FilterOutEmpty ls
  FilterOutEmpty (l ': ls) = l ': FilterOutEmpty ls

-- Type-level boolean OR
type family (||) (a :: Bool) (b :: Bool) :: Bool where
  'True || _ = 'True
  'False || b = b

-- Type-level list concatenation for lists of lists
type family Concat (lists :: [[k]]) :: [k] where
  Concat '[] = '[]
  Concat (l ': ls) = l ++ Concat ls