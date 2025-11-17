{-|
Module      : Modsefa.Core.Foundation.App
Description : Top-level application specification and validation.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module defines the core 'AppSpec' type class, the entry point for
defining a Modsefa application.

It co-locates 'AppSpec' with all its associated validation logic
('IsActionSpecValid') and App-aware type families ('AllManagedStates')
to resolve mutual dependencies between the definition of an App and the
logic that validates it.
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

module Modsefa.Core.Foundation.App
  ( -- * Application Specification
    AppSpec(..)
  , IsActionSpecValid
  , ExtractPlutusVersion
  ) where

import Data.Kind (Constraint, Type)
import Data.Type.Bool (If)
import Data.Type.Equality (type (==))
import GHC.Generics
  ( C, C1, D, D1, Generic(Rep), K1, M1, Meta(MetaCons, MetaData, MetaSel), S, U1, V1
  , (:+:), (:*:)
  )
import GHC.TypeLits
  ( CmpSymbol, ErrorMessage (ShowType, Text, (:<>:)), KnownSymbol, Symbol, TypeError
  )

import GeniusYield.Types (PlutusVersion)
import PlutusLedgerApi.V3 (POSIXTime, TxOutRef)

import Modsefa.Core.Foundation.Constraints
  ( ValidatorInValidatorList, ValidatorParamFieldInList
  )
import Modsefa.Core.Foundation.TypeFamilies
  ( ActionSpecConstraints, ActionSpecParameters, ActionSpecSteps, AllManagedStates
  , ExtractPlutusVersionFromValidators, FindParamInValidatorList, LookupParamInList
  )
import Modsefa.Core.Foundation.Types
  ( ActionStep(..), DerivationSource(ValidatorAddress, ValidatorHash), FieldSpec(..)
  , ParamDerivation(..), RefStrategy(OnlyAsUnique), StateSpec(DatumFields, Strategy)
  , TypedActionSpec, TypedConstraint(..), TypedOperation(..), TypedStateRef(..)
  , TypedValue(..)
  )
import Modsefa.Core.Foundation.Validator
  ( AllStatesHaveSpec, ValidatorDef(..), ValidatorSpec(ManagedStates, Params
  , ValidatorAppName)
  )


-- ============================================================================
-- * Application Specification
-- ============================================================================

-- | Core Application specification type class.
-- Defines the complete structure of a blockchain application including
-- its validators, states, transitions, and parameter configuration.
class
  ( InitialStateInStates app
  , ValidateAppInstanceParameters app
  , ValidateActionTransitions (ActionTransitions app) app
  ) => AppSpec (app :: Type) where

  -- | List of 'ValidatorDef's participating in this application.
  type Validators app :: [ValidatorDef]

  -- | List of application-level state names ('Symbol') used for high-level state machine tracking.
  type AppStates app :: [Symbol]

  -- | The initial state ('Symbol') of the application state machine. Must be in 'AppStates'.
  type InitialAppState app :: Symbol

  -- | Defines valid transitions between application states triggered by specific 'TypedActionSpec's.
  -- Format: @'(ActionSpec, FromStateName, ToStateName)@
  type ActionTransitions app :: [(TypedActionSpec, Symbol, Symbol)]

  -- | Instance-specific parameters for validators, linking validator names to parameter names.
  -- Format: @'(ValidatorName, ParameterName)@.
  type AppInstanceParameters app :: [(Symbol, Symbol)]

  -- | Defines rules for automatically deriving validator parameters from other sources within the application instance.
  type ParameterDerivations app :: [ParamDerivation]

-- ============================================================================
-- *. The Validation Layer Entry Point
-- ============================================================================

-- | Top-level validation constraint for a 'TypedActionSpec'.
-- An action specification @spec@ satisfies @'IsActionSpecValid' spec@ if and only if
-- its steps ('ValidateActionSteps'), constraints ('ValidateConstraints'), and
-- instance consistency ('VerifyInstanceConsistency') are all valid according
-- to the type-level rules defined in this module.
-- This constraint is checked by 'Modsefa.Core.Actions.mkTypedAction'.
class
  ( AppSpec app -- Requires the application context
  , ValidateActionSteps (ActionSpecSteps spec) app spec -- Validates the sequence of operations
  , ValidateConstraints (ActionSpecConstraints spec) app -- Validates the transaction-level constraints
  , VerifyInstanceConsistency (ActionSpecSteps spec) (ActionSpecConstraints spec) app -- Checks instance consistency
  ) => IsActionSpecValid (app :: Type) (spec :: TypedActionSpec)

-- | The sole instance of 'IsActionSpecValid'. It simply requires all the necessary
-- sub-validations (defined as superclasses) to hold. GHC's instance resolution
-- mechanism triggers the evaluation of these validation type families
instance
  ( AppSpec app
  , ValidateActionSteps (ActionSpecSteps spec) app spec
  , ValidateConstraints (ActionSpecConstraints spec) app
  , VerifyInstanceConsistency (ActionSpecSteps spec) (ActionSpecConstraints spec) app
  ) => IsActionSpecValid (app :: Type) (spec :: TypedActionSpec)

-- ============================================================================
-- * Top-Level Validation Helpers
-- ============================================================================

-- | Constraint ensuring initial state is valid
--
-- Validates that the application's initial state is one of the declared
-- application states.
type family InitialStateInStates (app :: Type) :: Constraint where
  InitialStateInStates app =
    If (IsElementOf (InitialAppState app) (AppStates app))
       (() :: Constraint)
       (TypeError ('Text "Initial state " ':<>: 'ShowType (InitialAppState app)
                   ':<>: 'Text " is not in application states"))

-- | Constraint validating application instance parameters
--
-- Ensures that the instance parameters reference valid validators and
-- parameter names.
type family ValidateAppInstanceParameters (app :: Type) :: Constraint where
  ValidateAppInstanceParameters app = ValidateInstanceParamList (AppInstanceParameters app) (Validators app)

-- Helper type families for validation
type family IsElementOf (elem :: k) (list :: [k]) :: Bool where
  IsElementOf elem (elem ': _) = 'True
  IsElementOf elem (_ ': rest) = IsElementOf elem rest
  IsElementOf _ '[] = 'False

type family ValidateInstanceParamList (params :: [(Symbol, Symbol)]) (validators :: [ValidatorDef]) :: Constraint where
  ValidateInstanceParamList '[] _ = ()
  ValidateInstanceParamList ('(validatorName, paramName) ': rest) validators =
    ( ValidateValidatorHasParam validatorName paramName validators
    , ValidateInstanceParamList rest validators
    )

type family ValidateValidatorHasParam (validatorName :: Symbol) (paramName :: Symbol) (validators :: [ValidatorDef]) :: Constraint where
  ValidateValidatorHasParam validatorName paramName validators =
    If (HasValidatorParam validatorName paramName validators)
       (() :: Constraint)
       (TypeError ('Text "Validator " ':<>: 'ShowType validatorName
                   ':<>: 'Text " does not have parameter " ':<>: 'ShowType paramName))

type family HasValidatorParam (validatorName :: Symbol) (paramName :: Symbol) (validators :: [ValidatorDef]) :: Bool where
  HasValidatorParam validatorName paramName ('Validator v ': rest) =
    If (ValidatorAppName v == validatorName)
       (IsElementOf paramName (ParamNames (Params v)))
       (HasValidatorParam validatorName paramName rest)
  HasValidatorParam _ _ '[] = 'False

type family ParamNames (params :: [(Symbol, Type)]) :: [Symbol] where
  ParamNames '[] = '[]
  ParamNames ('(name, _) ': rest) = name ': ParamNames rest

-- | Validates all actions in the 'ActionTransitions' list against the app.
-- This provides "late validation" for reusable actions and
-- performs state machine lifecycle checks.
type family ValidateActionTransitions (transitions :: [(TypedActionSpec, Symbol, Symbol)]) (app :: Type) :: Constraint where
  ValidateActionTransitions transitions app =
    ( AllStatesHaveSpec (AllManagedStates (Validators app))
    , ValidateActionTransitions' transitions app transitions
    )

-- | (Internal) Helper for ValidateActionTransitions
type family ValidateActionTransitions' (transitions :: [(TypedActionSpec, Symbol, Symbol)]) (app :: Type) (allTransitions :: [(TypedActionSpec, Symbol, Symbol)]) :: Constraint where
  ValidateActionTransitions' '[] _ _ = ()
  ValidateActionTransitions' (t ': rest) app allTransitions =
    ( -- 1. Check the transition itself
      CheckTransition t app allTransitions
      -- 2. Recurse
    , ValidateActionTransitions' rest app allTransitions
    )

-- | (Internal) Helper for ValidateActionTransitions'.
-- Checks an individual transition and its implications on the state machine.
type family CheckTransition (transition :: (TypedActionSpec, Symbol, Symbol)) (app :: Type) (allTransitions :: [(TypedActionSpec, Symbol, Symbol)]) :: Constraint where
  CheckTransition '(spec, from, to) app allTransitions =
    ( -- 1. Check that the action itself is valid
      IsActionSpecValid app spec
      -- 2. Check that the 'from' state is in the AppStates list
    , CheckStateInAppStates from (AppStates app)
      -- 3. Check that the 'to' state is in the AppStates list
    , CheckStateInAppStates to (AppStates app)
    -- 4. Lifecycle check
    , ValidateStateLifecycleForState to app allTransitions
    )

-- | (Internal) Helper for ValidateActionTransitions to check state membership
type family CheckStateInAppStates (state :: Symbol) (appStates :: [Symbol]) :: Constraint where
  CheckStateInAppStates state appStates =
    If (IsElementOf state appStates)
       (() :: Constraint)
       (TypeError ('Text "State '" ':<>: 'ShowType state ':<>: 'Text "' in ActionTransitions is not defined in AppStates."))

-- | Looks up the 'Type' of a specific parameter ('param') within a specific validator ('vName')
-- defined in the application ('app'). Used for type validation.
type family LookupValidatorParamType (vName :: Symbol) (param :: Symbol) (app :: Type) :: Type where
  LookupValidatorParamType vName param app = FindParamInValidatorList vName param (Validators app)

-- | Extracts the 'PlutusVersion' used by the application.
-- Assumes all validators in the app use the same version.
type family ExtractPlutusVersion (app :: Type) :: PlutusVersion where
  ExtractPlutusVersion app = ExtractPlutusVersionFromValidators (Validators app)

-- | Type-level check ensuring a validator named @vName@ in application @app@ has a parameter named @param@.
-- Produces a 'TypeError' if the validator or parameter is not found.
type family ValidatorParamFieldExists (vName :: Symbol) (param :: Symbol) (app :: Type) :: Constraint where
  ValidatorParamFieldExists vName param app =
    ValidatorParamFieldInList vName param (Validators app)

-- | Type-level check ensuring a validator named @vName@ exists within the 'Validators' list of application @app@.
-- Produces a 'TypeError' if the validator is not found.
type family ValidatorExistsInApp (vName :: Symbol) (app :: Type) :: Constraint where
  ValidatorExistsInApp vName app = ValidatorInValidatorList vName (Validators app)

-- ============================================================================
-- * Type-Level Instance Consistency Validation
-- ============================================================================
-- This section defines type families that trace validator dependencies
-- through parameters and derivations to ensure all states accessed within
-- a single action belong to the same logical application instance.

-- | Entry point: Ensures all states referenced in an action trace back to a common root anchor ('AppInstanceParameters').
-- Gathers all states, finds their roots, and checks if all non-empty roots are identical.
type family VerifyInstanceConsistency (steps :: [ActionStep]) (constraints :: [TypedConstraint]) (app :: Type) :: Constraint where
  VerifyInstanceConsistency steps constraints app =
    AllRootsEqual (GetAllInstanceRoots (CollectStates steps constraints) app)

-- | Collects all unique state tags mentioned in an action's steps and constraints.
type family CollectStates (steps :: [ActionStep]) (constraints :: [TypedConstraint]) :: [Type] where
  CollectStates steps constraints = RemoveDuplicates (CollectStatesFromSteps steps ++ CollectStatesFromConstraints constraints)

-- | Applies 'GetInstanceRoot' to each state tag in a list.
type family GetAllInstanceRoots (states :: [Type]) (app :: Type) :: [[(Symbol, Symbol)]] where
  GetAllInstanceRoots '[] _ = '[]
  GetAllInstanceRoots (s ': sts) app = GetInstanceRoot s app ': GetAllInstanceRoots sts app

-- | Finds the root 'AppInstanceParameters' anchor(s) for a single state tag 's'.
type family GetInstanceRoot (s :: Type) (app :: Type) :: [(Symbol, Symbol)] where
  GetInstanceRoot s app =
    TraceValidator (FindManagingValidator s (Validators app)) app

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
-- * The Validation Logic
-- ============================================================================
-- This section contains type families that perform detailed checks on operations,
-- field specifications, constraints, etc., ensuring they are well-formed and type-correct.

-- | (Internal) Recursively validates a list of 'ActionStep's against an 'AppSpec'.
type family ValidateActionSteps (steps :: [ActionStep]) (app :: Type) (spec :: TypedActionSpec) :: Constraint where
  ValidateActionSteps '[] _ _ = ()
  ValidateActionSteps (step ': rest) app spec =
    ( ValidateActionStep step app spec '[]
    , ValidateActionSteps rest app spec
    )

-- | (Internal) Validates a single 'ActionStep' by dispatching to 'ValidateOperation'.
type family ValidateActionStep (step :: ActionStep) (app :: Type) (spec :: TypedActionSpec) (mapContext :: [(Symbol, Type)]) :: Constraint where
  -- 'Op' and 'Let' just pass the context through
  ValidateActionStep ('Op op) app spec mapContext = ValidateOperation op app spec mapContext
  ValidateActionStep ('Let _ op) app spec mapContext = ValidateOperation op app spec mapContext

  -- 'Map' *creates* a new context
  ValidateActionStep ('Map op param constraints) app spec _ =
    ( -- 1. Compute ItemType on the fly and apply the Generic constraint
      Generic (ListItem (LookupParamInList param (ActionSpecParameters spec)))

      -- 2. Compute ItemFields on the fly and pass it as the new mapContext
      --    to the recursive ValidateOperation call.
    , ValidateOperation op app spec (GMapItemFields (Rep (ListItem (LookupParamInList param (ActionSpecParameters spec)))))
    )

-- | (Internal) Validates a single 'TypedOperation'.
-- Checks that the state is managed by a validator and that field specifications match the 'StateDatum'.
type family ValidateOperation (op :: TypedOperation) (app :: Type) (spec :: TypedActionSpec) (mapContext :: [(Symbol, Type)]) :: Constraint where
  ValidateOperation ('Create @s specs constraints) app spec mapContext =
    ( CheckValidatorManagesState s app
    , ValidateFieldSpecs s specs app spec mapContext
    , CheckFieldCompleteness s specs
    )
  ValidateOperation ('Update (ref :: TypedStateRef s) specs constraints) app spec mapContext =
    ( CheckValidatorManagesState s app
    , ValidateFieldSpecs s specs app spec mapContext
    , ValidateStateRef ref
    , CheckFieldCompleteness s specs
    )
  ValidateOperation ('Delete (ref :: TypedStateRef s) constraints) app spec mapContext =
    ( CheckValidatorManagesState s app
    , ValidateStateRef ref
    )
  ValidateOperation ('Reference (ref :: TypedStateRef s) constraints) app spec mapContext =
    ( CheckValidatorManagesState s app
    , ValidateStateRef ref
    )

-- | Extracts the element type from a list type.
type family ListItem (list :: Type) :: Type where
  ListItem [a] = a
  ListItem _ = TypeError ('Text "Parameter in 'Map' operation is not a list type.")

type family GMapItemFields (rep :: Type -> Type) :: [(Symbol, Type)] where
  GMapItemFields (M1 D _ f) = GMapItemFields f
  GMapItemFields (M1 C _ f) = GMapItemFields f
  GMapItemFields (f :*: g) = GMapItemFields f ++ GMapItemFields g
  GMapItemFields (M1 S ('MetaSel ('Just name) _ _ _) (K1 _ t)) = '[ '(name, t) ]
  GMapItemFields U1 = '[]
  GMapItemFields V1 = '[]
  GMapItemFields (K1 _ _) = '[] -- Should not happen for record fields

-- | (Internal) Validates that all fields in a state's datum are
-- | explicitly handled ('SetTo' or 'Preserve') in an operation.
type family CheckFieldCompleteness (s :: Type) (specs :: [FieldSpec]) :: Constraint where
  CheckFieldCompleteness s specs =
    ( StateSpec s
    , CheckAllFieldsCovered (DatumFields s) (GetFieldNamesFromSpecs specs)
    )

-- | Helper to get a list of field names from FieldSpecs
type family GetFieldNamesFromSpecs (specs :: [FieldSpec]) :: [Symbol] where
  GetFieldNamesFromSpecs '[] = '[]
  GetFieldNamesFromSpecs ('SetTo name _ ': rs) = name ': GetFieldNamesFromSpecs rs
  GetFieldNamesFromSpecs ('Preserve name ': rs) = name ': GetFieldNamesFromSpecs rs

-- | Helper to check if all fields from DatumFields are in the spec list
type family CheckAllFieldsCovered (fields :: [(Symbol, Type)]) (specNames :: [Symbol]) :: Constraint where
  CheckAllFieldsCovered '[] _ = () -- All datum fields have been checked
  CheckAllFieldsCovered ('(name, _) ': rs) specNames =
    ( If (IsElementOf name specNames)
         (() :: Constraint)
         (TypeError ('Text "Operation is missing a specification for field '"
                    ':<>: 'ShowType name ':<>: 'Text "'."
                    ':<>: 'Text " All fields must be handled by 'SetTo' or 'Preserve'."))
    , CheckAllFieldsCovered rs specNames
    )

-- | (Internal) Type class for validating a list of 'FieldSpec's against a state tag 's'.
-- This class validates by checking against the type-level 'DatumFields'
-- from the 'StateSpec s' instance, removing the need for 'HasField'.
class (StateSpec s, AppSpec app) => ValidateFieldSpecs (s :: Type) (specs :: [FieldSpec]) (app :: Type) (spec :: TypedActionSpec) (mapContext :: [(Symbol, Type)])
instance (StateSpec s, AppSpec app) => ValidateFieldSpecs s '[] app spec mapContext

-- (Modify all 'ValidateFieldSpecs' instances to pass 'app' and 'spec' to 'ValidateTypedValue')
instance ( ValidateTypedValue ('ParamValue p) app spec (FindFieldType field (DatumFields s)) mapContext
         , CheckFieldExists field (DatumFields s)
         , ValidateFieldSpecs s rest app spec mapContext
         )
  => ValidateFieldSpecs s ('SetTo field ('ParamValue p) ': rest) app spec mapContext

instance ( fieldType ~ FindFieldType field (DatumFields s)
         , ValidateTypedValue ('EnumValue t e) app spec fieldType mapContext
         , ValidateFieldSpecs s rest app spec mapContext
         )
  => ValidateFieldSpecs s ('SetTo field ('EnumValue t e) ': rest) app spec mapContext

instance ( fieldType ~ FindFieldType field (DatumFields s)
         , ValidateTypedValue ('StateFieldValue label fname) app spec fieldType mapContext
         , ValidateFieldSpecs s rest app spec mapContext
         ) => ValidateFieldSpecs s ('SetTo field ('StateFieldValue label fname) ': rest) app spec mapContext

instance ( ValidateTypedValue 'CurrentTime app spec POSIXTime mapContext
         , FindFieldType field (DatumFields s) ~ POSIXTime
         , ValidateFieldSpecs s rest app spec mapContext
         ) => ValidateFieldSpecs s ('SetTo field 'CurrentTime ': rest) app spec mapContext

instance ( fieldType ~ FindFieldType field (DatumFields s)
         , ValidateTypedValue ('AddValue v1 v2) app spec fieldType mapContext
         , ValidateFieldSpecs s rest app spec mapContext
         ) => ValidateFieldSpecs s ('SetTo field ('AddValue v1 v2) ': rest) app spec mapContext

instance (CheckFieldExists field (DatumFields s), ValidateFieldSpecs s rest app spec mapContext)
  => ValidateFieldSpecs s ('Preserve field ': rest) app spec mapContext

instance ( fieldType ~ FindFieldType field (DatumFields s)
         , ValidateTypedValue ('SubtractValue v1 v2) app spec fieldType mapContext
         , ValidateFieldSpecs s rest app spec mapContext
         ) => ValidateFieldSpecs s ('SetTo field ('SubtractValue v1 v2) ': rest) app spec mapContext

instance ( fieldType ~ FindFieldType field (DatumFields s)
         , ValidateTypedValue ('MultiplyValue v1 v2) app spec fieldType mapContext
         , ValidateFieldSpecs s rest app spec mapContext
         ) => ValidateFieldSpecs s ('SetTo field ('MultiplyValue v1 v2) ': rest) app spec mapContext

instance ( fieldType ~ FindFieldType field (DatumFields s)
         , ValidateTypedValue ('DivideValue v1 v2) app spec fieldType mapContext
         , ValidateFieldSpecs s rest app spec mapContext
         ) => ValidateFieldSpecs s ('SetTo field ('DivideValue v1 v2) ': rest) app spec mapContext


type family ValidateTypedValue (value :: TypedValue) (app :: Type) (spec :: TypedActionSpec) (expectedType :: Type) (mapContext :: [(Symbol, Type)]) :: Constraint where
  -- Case 1: ParamValue - Pass 'mapContext' to 'CheckParamExists'
  ValidateTypedValue ('ParamValue name) app spec expectedType mapContext =
    ( CheckParamExists name (ActionSpecParameters spec) (ParameterDerivations app) (AppInstanceParameters app) mapContext
    -- TODO: Check resolved type against expectedType
    )

  -- (All other cases just pass 'mapContext' down recursively or ignore it)
  ValidateTypedValue ('EnumValue t constructor) app spec expectedType _ =
    ( t ~ expectedType
    , CheckEnumValue t constructor
    )
  ValidateTypedValue ('IntValue n) app spec Integer _ = ()
  ValidateTypedValue ('IntValue n) app spec expectedType _ =
    TypeError ('Text "Literal 'IntValue' expects field of type Integer, but got " ':<>: 'ShowType expectedType)
  ValidateTypedValue 'CurrentTime app spec POSIXTime _ = ()
  ValidateTypedValue 'CurrentTime app spec expectedType _ =
    TypeError ('Text "'CurrentTime' provides POSIXTime, but field has type " ':<>: 'ShowType expectedType)
  ValidateTypedValue ('StateFieldValue label field) app spec expectedType _ =
    ( CheckLabelExists label (ActionSpecSteps spec)
    )
  ValidateTypedValue ('AddValue v1 v2) app spec expectedType mapContext =
    ( ValidateTypedValue v1 app spec expectedType mapContext
    , ValidateTypedValue v2 app spec expectedType mapContext
    )
  ValidateTypedValue ('SubtractValue v1 v2) app spec expectedType mapContext =
    ( ValidateTypedValue v1 app spec expectedType mapContext
    , ValidateTypedValue v2 app spec expectedType mapContext
    )
  ValidateTypedValue ('MultiplyValue v1 v2) app spec expectedType mapContext =
    ( ValidateTypedValue v1 app spec expectedType mapContext
    , ValidateTypedValue v2 app spec expectedType mapContext
    )
  ValidateTypedValue ('DivideValue v1 v2) app spec expectedType mapContext =
    ( ValidateTypedValue v1 app spec expectedType mapContext
    , ValidateTypedValue v2 app spec expectedType mapContext
    )

-- (Add these new helper families)
-- | Helper for checking 'Let' bindings
type family CheckLabelExists (label :: Symbol) (steps :: [ActionStep]) :: Constraint where
  CheckLabelExists label steps =
    If (LabelInSteps label steps)
       (() :: Constraint)
       (TypeError ('Text "'StateFieldValue' references label '"
                  ':<>: 'ShowType label ':<>: 'Text "' which is not defined by a 'Let' step in this action."))

type family LabelInSteps (label :: Symbol) (steps :: [ActionStep]) :: Bool where
  LabelInSteps label '[] = 'False
  LabelInSteps label ('Let label _ ': rs) = 'True
  LabelInSteps label (_ ': rs) = LabelInSteps label rs

-- | (Internal) Helper to check if a parameter name exists in any source
type family CheckParamExists (name :: Symbol) (actionParams :: [(Symbol, Type)]) (derivations :: [ParamDerivation]) (instanceParams :: [(Symbol, Symbol)]) (mapContext :: [(Symbol, Type)]) :: Constraint where
  CheckParamExists name actionParams derivations instanceParams mapContext =
    If (ParamInList' name actionParams || ParamInDerivations' name derivations || ParamInInstanceParams' name instanceParams || ParamInList' name mapContext)
       (() :: Constraint)
       (TypeError ('Text "Parameter '" ':<>: 'ShowType name ':<>: 'Text "' used in 'ParamValue' is not defined in"
                  ':<>: 'Text " action parameters, parameter derivations, app instance parameters, or 'Map' context."))

-- Helper: Check '[(Symbol, Type)]' list
type family ParamInList' (name :: Symbol) (params :: [(Symbol, Type)]) :: Bool where
  ParamInList' name '[] = 'False
  ParamInList' name ('(name, _) ': rs) = 'True
  ParamInList' name (_ ': rs) = ParamInList' name rs

-- Helper: Check '[ParamDerivation]' list
type family ParamInDerivations' (name :: Symbol) (derivs :: [ParamDerivation]) :: Bool where
  ParamInDerivations' name '[] = 'False
  ParamInDerivations' name ('DeriveParam name _ ': rs) = 'True
  ParamInDerivations' name (_ ': rs) = ParamInDerivations' name rs

-- Helper: Check '[(Symbol, Symbol)]' list for instance params
type family ParamInInstanceParams' (name :: Symbol) (params :: [(Symbol, Symbol)]) :: Bool where
  ParamInInstanceParams' name '[] = 'False
  ParamInInstanceParams' name ('(_, name) ': rs) = 'True
  ParamInInstanceParams' name (_ ': rs) = ParamInInstanceParams' name rs

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

-- | Helper to extract the state tag ('Type') from a 'TypedStateRef'.
type family GetStateFromRef (ref :: TypedStateRef s) :: Type where
  GetStateFromRef (ref :: TypedStateRef s) = s

-- | (Internal) Validates a single 'TypedConstraint'. Checks referenced validators/parameters exist,
-- referenced state fields exist, type compatibility, etc.
type family ValidateConstraint (constraint :: TypedConstraint) (app :: Type) :: Constraint where

  -- MustBeSignedByState: Check state is managed, field exists in StateSpec, ref is valid.
  ValidateConstraint ('MustBeSignedByState (ref :: TypedStateRef st) field) app =
    ( CheckValidatorManagesState (GetStateFromRef ref) app
    , StateSpec (GetStateFromRef ref)
    , CheckFieldExists field (DatumFields (GetStateFromRef ref))
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

-- | (Internal) Type class check: Ensures a state tag 's' is listed in the 'ManagedStates'
-- of at least one validator within the application.
class CheckValidatorManagesState (s :: Type) (app :: Type)
instance (AppSpec app, CheckValidatorManagesState' s (Validators app)) => CheckValidatorManagesState s app

-- | Helper for 'CheckValidatorManagesState'.
class CheckValidatorManagesState' (s :: Type) (vDefs :: [ValidatorDef])
instance (TypeError ('Text "State " ':<>: 'ShowType s ':<>: 'Text " not managed by any validator in AppSpec."))
  => CheckValidatorManagesState' s '[]
instance
  ( ValidatorSpec v
  , If (IsStateInList s (ManagedStates v))
      (() :: Constraint)
      (CheckValidatorManagesState' s rest)
  ) => CheckValidatorManagesState' s ('Validator v ': rest)

-- ============================================================================
-- * Lower-Level Generic Helpers
-- ============================================================================
-- These are supporting type families used by the main validation logic above.

-- | Type-level check if a state tag 's' is present in a list of state tags.
type family IsStateInList (s :: Type) (states :: [Type]) :: Bool where
  IsStateInList s (s ': _) = 'True
  IsStateInList s (_ ': rest) = IsStateInList s rest
  IsStateInList s '[] = 'False

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
type family CollectStatesFromSteps (steps :: [ActionStep]) :: [Type] where
  CollectStatesFromSteps '[] = '[]
  CollectStatesFromSteps ('Op op ': ss) = GetOpStates op ++ CollectStatesFromSteps ss
  CollectStatesFromSteps ('Let _ op ': ss) = GetOpStates op ++ CollectStatesFromSteps ss
  CollectStatesFromSteps ('Map op _ _ ': ss) = GetOpStates op ++ CollectStatesFromSteps ss

type family CollectStatesFromConstraints (constraints :: [TypedConstraint]) :: [Type] where
  CollectStatesFromConstraints '[] = '[]
  CollectStatesFromConstraints (c ': cs) = GetConstraintStates c ++ CollectStatesFromConstraints cs

type family GetOpStates (op :: TypedOperation) :: [Type] where
  GetOpStates ('Create @s _ _) = '[ s ]
  GetOpStates ('Update ref _ _) = '[GetStateFromRef ref]
  GetOpStates ('Delete ref _) = '[GetStateFromRef ref]
  GetOpStates ('Reference ref _) = '[GetStateFromRef ref]

type family GetConstraintStates (c :: TypedConstraint) :: [Type] where
  GetConstraintStates ('MustBeSignedByState ref _) = '[GetStateFromRef ref]
  GetConstraintStates ('MustNotExist ref) = '[GetStateFromRef ref]
  GetConstraintStates ('MustExist ref) = '[GetStateFromRef ref]
  GetConstraintStates ('ExactlyN _ ref) = '[GetStateFromRef ref]
  GetConstraintStates ('AtMostN _ ref) = '[GetStateFromRef ref]
  GetConstraintStates ('AtLeastN _ ref) = '[GetStateFromRef ref]
  GetConstraintStates ('PreserveStateField ref _) = '[GetStateFromRef ref]
  GetConstraintStates ('RequireStateValue ref _ _) = '[GetStateFromRef ref]
  GetConstraintStates _ = '[]

-- | (Internal) Helper for instance consistency: Finds the 'ValidatorDef' managing a given state tag 's'.
type family FindManagingValidator (s :: Type) (vs :: [ValidatorDef]) :: ValidatorDef where
  FindManagingValidator s ('Validator v ': vs) =
    If (IsStateInList s (ManagedStates v))
       ('Validator v)
       (FindManagingValidator s vs)
  FindManagingValidator s '[] = TypeError ('Text "Internal validation error: Could not find validator for state " ':<>: 'ShowType s)

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

-- ============================================================================
-- * StateSpec-based Validation Helpers
-- ============================================================================

-- | Type family to check if a field exists in a 'DatumFields' list.
type family CheckFieldExists (fieldName :: Symbol) (fields :: [(Symbol, Type)]) :: Constraint where
  CheckFieldExists fieldName ('(fieldName, _) ': _) = ()
  CheckFieldExists fieldName (_ ': rest) = CheckFieldExists fieldName rest
  CheckFieldExists fieldName '[] = TypeError ('Text "Field '" ':<>: 'ShowType fieldName ':<>: 'Text "' not found in StateSpec DatumFields.")

-- | Type family to find the type of a field by name in a 'DatumFields' list.
type family FindFieldType (fieldName :: Symbol) (fields :: [(Symbol, Type)]) :: Type where
  FindFieldType fieldName ('(fieldName, fieldType) ': _) = fieldType
  FindFieldType fieldName (_ ': rest) = FindFieldType fieldName rest
  FindFieldType fieldName '[] = TypeError ('Text "Field '" ':<>: 'ShowType fieldName ':<>: 'Text "' not found in StateSpec DatumFields.")

-- ============================================================================
-- * Type-Level State Machine Lifecycle Validation
-- ============================================================================
-- Note: These checks are conservative and based on one-step reachability.
-- They check for immediate violations after a transition.

-- | (Internal) Validates the lifecycle of states based on transitions *to* and *from* a given AppState.
-- This is the entry point we will add to CheckTransition in the next step.
type family ValidateStateLifecycleForState (appState :: Symbol) (app :: Type) (allTransitions :: [(TypedActionSpec, Symbol, Symbol)]) :: Constraint where
  ValidateStateLifecycleForState appState app allTransitions =
    ( -- 1. Check for Uniqueness violations
      CheckUniquenessViolations
        (GetCreatedUniqueStatesT (GetIncomingTransitions appState allTransitions))
        (GetOutgoingTransitions appState allTransitions)

    -- 2. Check for MustNotExist violations
    , CheckMustNotExistViolations
        (GetCreatedStatesT (GetIncomingTransitions appState allTransitions))
        (GetOutgoingTransitions appState allTransitions)
    )

-- | (Internal) Check for uniqueness violations:
-- If we *just created* a unique state (in 'createdUniqueStates'),
-- can we *immediately* create it again (from 'outgoingTransitions')?
type family CheckUniquenessViolations (createdUniqueStates :: [Type]) (outgoingTransitions :: [(TypedActionSpec, Symbol, Symbol)]) :: Constraint where
  CheckUniquenessViolations '[] _ = () -- No unique states created, so no violation
  CheckUniquenessViolations (s ': rs) outgoingTransitions =
    ( -- Check for this state 's'
      If (ListContains (GetCreatedUniqueStatesT outgoingTransitions) s)
         (If (ListContains (GetDeletedStatesT outgoingTransitions) s)
             (() :: Constraint) -- It's created again, but also deleted, so this *might* be fine. (Conservative pass)
             (TypeError ('Text "State Machine Validation Error: State " ':<>: 'ShowType s
                         ':<>: 'Text " is a unique state ('OnlyAsUnique') and is created by a transition."
                         ':<>: 'Text " An immediately reachable action also creates this state without a guaranteed deletion path."))
         )
         (() :: Constraint) -- Not created again, so no violation
    , -- Check the rest of the states
      CheckUniquenessViolations rs outgoingTransitions
    )

-- | (Internal) Check for MustNotExist violations:
-- If we *just created* a state (in 'createdStates'),
-- can we *immediately* call an action that requires it *not* to exist?
type family CheckMustNotExistViolations (createdStates :: [Type]) (outgoingTransitions :: [(TypedActionSpec, Symbol, Symbol)]) :: Constraint where
  CheckMustNotExistViolations '[] _ = () -- No states created, so no violation
  CheckMustNotExistViolations (s ': rs) outgoingTransitions =
    ( -- Check for this state 's'
      If (ListContains (GetMustNotExistStatesT outgoingTransitions) s)
         (If (ListContains (GetDeletedStatesT outgoingTransitions) s)
             (() :: Constraint) -- It's required not to exist, but a deletion path exists. (Conservative pass)
             (TypeError ('Text "State Machine Validation Error: State " ':<>: 'ShowType s
                         ':<>: 'Text " is created by a transition."
                         ':<>: 'Text " An immediately reachable action has a 'MustNotExist' constraint for this state, but no guaranteed deletion path exists."))
         )
         (() :: Constraint) -- No 'MustNotExist' constraint, so no violation
    , -- Check the rest of the states
      CheckMustNotExistViolations rs outgoingTransitions
    )

-- | Helper: Get all transitions starting from 'appState'
type family GetOutgoingTransitions (appState :: Symbol) (allTransitions :: [(TypedActionSpec, Symbol, Symbol)]) :: [(TypedActionSpec, Symbol, Symbol)] where
  GetOutgoingTransitions _ '[] = '[]
  GetOutgoingTransitions appState ('(spec, from, to) ': rs) =
    If (from == appState)
       ('(spec, from, to) ': GetOutgoingTransitions appState rs)
       (GetOutgoingTransitions appState rs)

-- | Helper: Get all transitions ending in 'appState'
type family GetIncomingTransitions (appState :: Symbol) (allTransitions :: [(TypedActionSpec, Symbol, Symbol)]) :: [(TypedActionSpec, Symbol, Symbol)] where
  GetIncomingTransitions _ '[] = '[]
  GetIncomingTransitions appState ('(spec, from, to) ': rs) =
    If (to == appState)
       ('(spec, from, to) ': GetIncomingTransitions appState rs)
       (GetIncomingTransitions appState rs)

-- | Helper: Get all states created in a list of transitions
type family GetCreatedStatesT (transitions :: [(TypedActionSpec, Symbol, Symbol)]) :: [Type] where
  GetCreatedStatesT '[] = '[]
  GetCreatedStatesT ('(spec, _, _) ': rs) = GetCreatedStatesFromSteps (ActionSpecSteps spec) ++ GetCreatedStatesT rs

-- | Helper: Get all *unique* states created in a list of transitions
type family GetCreatedUniqueStatesT (transitions :: [(TypedActionSpec, Symbol, Symbol)]) :: [Type] where
  GetCreatedUniqueStatesT '[] = '[]
  GetCreatedUniqueStatesT ('(spec, _, _) ': rs) = FilterUnique (GetCreatedStatesFromSteps (ActionSpecSteps spec)) ++ GetCreatedUniqueStatesT rs

-- | Helper: Get all states deleted in a list of transitions
type family GetDeletedStatesT (transitions :: [(TypedActionSpec, Symbol, Symbol)]) :: [Type] where
  GetDeletedStatesT '[] = '[]
  GetDeletedStatesT ('(spec, _, _) ': rs) = GetDeletedStatesFromSteps (ActionSpecSteps spec) ++ GetDeletedStatesT rs

-- | Helper: Get all MustNotExist states in a list of transitions
type family GetMustNotExistStatesT (transitions :: [(TypedActionSpec, Symbol, Symbol)]) :: [Type] where
  GetMustNotExistStatesT '[] = '[]
  GetMustNotExistStatesT ('(spec, _, _) ': rs) = GetMustNotExistStatesFromConstraints (ActionSpecConstraints spec) ++ GetMustNotExistStatesT rs

-- | Helper: Get state's Strategy (requires 'StateSpec s' constraint in scope)
type family GetStateStrategy (s :: Type) :: RefStrategy where
  GetStateStrategy s = Strategy s

-- | Helper: Check if a state is unique
type family IsUniqueState (s :: Type) :: Bool where
  IsUniqueState s = (GetStateStrategy s == 'OnlyAsUnique)

-- | Helper: Filter a list of states to only include unique ones
type family FilterUnique (states :: [Type]) :: [Type] where
  FilterUnique '[] = '[]
  FilterUnique (s ': rs) = If (IsUniqueState s) (s ': FilterUnique rs) (FilterUnique rs)

-- | Helper: Type-level list membership
type family ListContains (l :: [k]) (elem :: k) :: Bool where
  ListContains '[] _ = 'False
  ListContains (x ': rs) x = 'True
  ListContains (_ ': rs) x = ListContains rs x

-- | Helper: GetCreatedStatesFromSteps
type family GetCreatedStatesFromSteps (steps :: [ActionStep]) :: [Type] where
  GetCreatedStatesFromSteps '[] = '[]
  GetCreatedStatesFromSteps ('Op ('Create @s _ _) ': rs) = s ': GetCreatedStatesFromSteps rs
  GetCreatedStatesFromSteps ('Let _ ('Create @s _ _) ': rs) = s ': GetCreatedStatesFromSteps rs
  GetCreatedStatesFromSteps ('Map ('Create @s _ _) _ _ ': rs) = s ': GetCreatedStatesFromSteps rs
  GetCreatedStatesFromSteps (_ ': rs) = GetCreatedStatesFromSteps rs

-- | Helper: GetDeletedStatesFromSteps
type family GetDeletedStatesFromSteps (steps :: [ActionStep]) :: [Type] where
  GetDeletedStatesFromSteps '[] = '[]
  GetDeletedStatesFromSteps ('Op ('Delete (ref :: TypedStateRef s) _) ': rs) = s ': GetDeletedStatesFromSteps rs
  GetDeletedStatesFromSteps ('Let _ ('Delete (ref :: TypedStateRef s) _) ': rs) = s ': GetDeletedStatesFromSteps rs
  GetDeletedStatesFromSteps ('Map ('Delete (ref :: TypedStateRef s) _) _ _ ': rs) = s ': GetDeletedStatesFromSteps rs
  GetDeletedStatesFromSteps (_ ': rs) = GetDeletedStatesFromSteps rs

-- | Helper: GetMustNotExistStatesFromConstraints
type family GetMustNotExistStatesFromConstraints (constraints :: [TypedConstraint]) :: [Type] where
  GetMustNotExistStatesFromConstraints '[] = '[]
  GetMustNotExistStatesFromConstraints ('MustNotExist ref ': rs) = GetStateFromRef ref ': GetMustNotExistStatesFromConstraints rs
  GetMustNotExistStatesFromConstraints (_ ': rs) = GetMustNotExistStatesFromConstraints rs