{-|
Module      : Modsefa.Core.Singletons.Auto
Description : Automatic singleton derivation for Modsefa specifications.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC (requires TypeApplications, ScopedTypeVariables, UndecidableInstances, etc.)

This module provides type classes (e.g., 'AutoSingletonValidators', 'AutoSingletonActionSpec')
and instances that allow the automatic generation of singleton values
(like 'SAppSpec', 'SValidator', 'SActionSpec') directly from their corresponding
type-level specifications ('AppSpec', 'ValidatorSpec', 'TypedActionSpec', etc.).

The primary entry point is 'autoSingletonFull', which leverages these type classes
to construct a complete 'SAppSpec' value from just the application type @app@,
provided @app@ has an 'AppSpec' instance and its components satisfy the necessary
auto-singleton constraints. This significantly reduces the boilerplate required
to work with Modsefa specifications at runtime.
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Automatic singleton derivation for the Modsefa library
--
-- This module provides type classes and instances for automatically
-- generating singleton values from type-level specifications. The core
-- function `autoSingletonFull` can generate complete application singletons
-- from just the type-level AppSpec.
--
-- Organization:
--   1. Main Entry Point
--   2. Application-Level Auto Generation
--   3. Validator Auto Generation  
--   4. Action & Operation Auto Generation
--   5. Field & Constraint Auto Generation
--   6. State & Reference Auto Generation
--   7. Parameter & Derivation Auto Generation
--   8. State Property Auto Generation
module Modsefa.Core.Singletons.Auto
  ( -- * Main Entry Point
    autoSingletonFull
    
    -- * Application-Level Classes
  , AutoSingletonValidators(..)
  , AutoSingletonAppStateList(..)
  , AutoSingletonInitialAppState(..)
  , AutoSingletonActionTransitionList(..)
    
    -- * Action & Operation Classes
  , AutoSingletonActionSpec(..)
  , AutoSingletonActionStepList(..)
  , AutoSingletonActionStep(..)

    -- * Parameter & Derivation Classes
  , AutoSingletonAppInstanceParams(..)
  , AutoSingletonParamDerivationList(..)
  , AutoSingletonParamList(..)

    -- * State Properties
  , AutoSingletonStateSpec(..)
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable)
import GHC.Generics (Rep)
import GHC.TypeLits (KnownNat, KnownSymbol, Symbol)

import GeniusYield.Types (PlutusVersion(..))
import PlutusLedgerApi.V3 (ToData)

import Modsefa.Core.Foundation
  ( ActionStep(..), AppSpec(..), CollectionConstraint(..), DerivationSource(..)
  , FieldSpec(..), GExtractField, InstanceType(..), ParamDerivation(..)
  , ParamsToValue, RefStrategy(..), SpecPolicySource(..), SpecStateIdentifier(..)
  , StateDatum, StateSpec(..), TypedActionSpec(..), TypedConstraint(..)
  , TypedOperation(..), TypedPredicate(And, FieldEquals), TypedStateRef(..)
  , TypedValue(..), ValidatorDef(..), ValidatorSpec(..)
  )

import Modsefa.Core.Singletons.Types
  ( FromEnumValue, GBuildDatumFromSpecs, SActionSpec(..), SActionSpecList(..)
  , SActionStep(..), SActionStepList(..), SActionTransition(..)
  , SActionTransitionList(..), SAppInstanceParamList(..), SAppSpec(..)
  , SAppStateList(..), SCollectionConstraint(..), SCollectionConstraintList(..)
  , SConstraint(..), SConstraintList(..), SDatumField(..), SDatumFieldList(..)
  , SDerivationSource(..), SFieldSpec(..), SFieldSpecList(..), SInitialAppState(..)
  , SInstanceType(..), SMappability(..), SOperation(..), SOperationList(..)
  , SParamDerivation(..), SParamDerivationList(..), SParamList(..)
  , SPlutusVersion(..), SPolicySource(..), SPredicate(..), SRefStrategy(..)
  , SStateIdentifier(..), SStateList(..), SStateRef(..), SStateSpec(..)
  , STypedValue(..), SValidator(..), SValidatorList(..)
  )


-- ============================================================================
-- 1. MAIN ENTRY POINT
-- ============================================================================

-- | Automatically generates a complete 'SAppSpec' singleton value for a given application type @app@.
-- Requires that @app@ has an 'AppSpec' instance and that all components of the spec
-- (validators, states, actions, parameters, derivations) satisfy their respective
-- 'AutoSingleton*' type class constraints. This function serves as the primary way
-- to obtain a value-level representation of the entire application specification.
autoSingletonFull :: forall app.
  ( AppSpec app
  , KnownSymbol (InitialAppState app)
  , AutoSingletonValidators (Validators app)
  , AutoSingletonAppStateList (AppStates app)
  , AutoSingletonInitialAppState (InitialAppState app)
  , AutoSingletonActionTransitionList (ActionTransitions app)
  , AutoSingletonAppInstanceParams (AppInstanceParameters app)
  , AutoSingletonParamDerivationList (ParameterDerivations app)
  ) => SAppSpec app
autoSingletonFull = SAppSpec 
  autoSingletonValidators
  autoSingletonAppStateList
  autoSingletonInitialAppState
  autoSingletonActionTransitionList
  autoSingletonAppInstanceParams
  autoSingletonParamDerivationList

-- ============================================================================
-- 2. APPLICATION-LEVEL AUTO GENERATION
-- ============================================================================

-- | Type class for automatically deriving an 'SValidatorList' from a type-level list of 'ValidatorDef's.
class AutoSingletonValidators (validators :: [ValidatorDef]) where
  -- | Method to produce the 'SValidatorList' singleton.
  autoSingletonValidators :: SValidatorList validators

-- | Base case: An empty list of validators maps to 'SVNil'.
instance AutoSingletonValidators '[] where
  autoSingletonValidators = SVNil

-- | Recursive case: Derives the 'SValidator' for the head and combines it with the derived tail.
-- Requires 'AutoSingletonValidator' for the head and 'AutoSingletonValidators' for the tail.
instance ( ValidatorSpec v
         , Typeable v
         , AutoSingletonValidator v -- Constraint to derive the head SValidator
         , AutoSingletonValidators rest -- Constraint to derive the tail SValidatorList
         , Typeable (ParamsToValue (Params v)) -- Needed for SValidator constraints
         , Show (ParamsToValue (Params v)) -- Needed for SValidator constraints
         ) => AutoSingletonValidators ('Validator v ': rest) where
  autoSingletonValidators = SVCons autoSingletonValidator autoSingletonValidators

-- | Type class for automatically deriving an 'SValidator' singleton from a validator type @v@.
class AutoSingletonValidator (v :: Type) where
  -- | Method to produce the 'SValidator' singleton.
  autoSingletonValidator :: SValidator v

-- | Instance for 'AutoSingletonValidator'. Derives all components of the 'SValidator'
-- (params, states, version, instance type) using their respective 'AutoSingleton*' classes.
instance ( ValidatorSpec v
         , KnownSymbol (ValidatorAppName v)
         , AutoSingletonParamList (Params v) -- Derive SParamList
         , AutoSingletonStateList (ManagedStates v) -- Derive SStateList
         , AutoSingletonPlutusVersion (ValidatorPlutusVersion v) -- Derive SPlutusVersion
         , AutoSingletonInstanceType (ValidatorInstanceType v) -- Derive SInstanceType
         ) => AutoSingletonValidator v where
  autoSingletonValidator = SValidator 
    autoSingletonParamList
    autoSingletonStateList
    autoSingletonPlutusVersion
    autoSingletonInstanceType

-- | Type class for automatically deriving an 'SAppStateList' from a type-level list of 'Symbol's.
class AutoSingletonAppStateList (states :: [Symbol]) where
  -- | Method to produce the 'SAppStateList' singleton.
  autoSingletonAppStateList :: SAppStateList states

-- | Base case: An empty list of states maps to 'SASLNil'.
instance AutoSingletonAppStateList '[] where
  autoSingletonAppStateList = SASLNil

-- | Recursive case: Creates a 'Proxy' for the head state and combines it with the derived tail.
instance (KnownSymbol state, AutoSingletonAppStateList rest) => AutoSingletonAppStateList (state ': rest) where
  autoSingletonAppStateList = SASLCons (Proxy @state) (autoSingletonAppStateList @rest)

-- | Type class for automatically deriving an 'SInitialAppState' from a 'Symbol'.
class AutoSingletonInitialAppState (state :: Symbol) where
  -- | Method to produce the 'SInitialAppState' singleton.
  autoSingletonInitialAppState :: SInitialAppState state

-- | Instance creates a 'Proxy' for the initial state symbol.
instance (KnownSymbol state) => AutoSingletonInitialAppState state where
  autoSingletonInitialAppState = SInitialAppState (Proxy @state)

-- | Type class for automatically deriving an 'SActionTransitionList' from the 'ActionTransitions' list.
class AutoSingletonActionTransitionList (transitions :: [(TypedActionSpec, Symbol, Symbol)]) where
  -- | Method to produce the 'SActionTransitionList' singleton.
  autoSingletonActionTransitionList :: SActionTransitionList app transitions

-- | Base case: An empty list of transitions maps to 'SATLNil'.
instance AutoSingletonActionTransitionList '[] where
  autoSingletonActionTransitionList = SATLNil

-- | Recursive case: Derives the 'SActionTransition' for the head and combines it with the derived tail.
instance ( AutoSingletonActionTransition transition -- Derive head SActionTransition
         , AutoSingletonActionTransitionList rest -- Derive tail SActionTransitionList
         ) => AutoSingletonActionTransitionList (transition ': rest) where
  autoSingletonActionTransitionList = SATLCons autoSingletonActionTransition autoSingletonActionTransitionList

-- | Type class for automatically deriving an 'SActionTransition' singleton from a transition tuple.
class AutoSingletonActionTransition (transition :: (TypedActionSpec, Symbol, Symbol)) where
  -- | Method to produce the 'SActionTransition' singleton.
  autoSingletonActionTransition :: SActionTransition app transition

-- | Instance derives the 'SActionSpec' and creates proxies for the state names.
instance ( AutoSingletonActionSpec action -- Derive the SActionSpec
         , KnownSymbol from -- Constraint for 'from' state name
         , KnownSymbol to -- Constraint for 'to' state name
         ) => AutoSingletonActionTransition '(action, from, to) where
  autoSingletonActionTransition = SActionTransition autoSingletonActionSpec (Proxy @from) (Proxy @to)

-- ============================================================================
-- 3. VALIDATOR AUTO GENERATION
-- ============================================================================

-- | Type class for automatically deriving an 'SPlutusVersion' singleton.
class AutoSingletonPlutusVersion (pv :: PlutusVersion) where
  -- | Method to produce the 'SPlutusVersion' singleton.
  autoSingletonPlutusVersion :: SPlutusVersion pv

instance AutoSingletonPlutusVersion 'PlutusV1 where
  autoSingletonPlutusVersion = SPlutusV1

instance AutoSingletonPlutusVersion 'PlutusV2 where
  autoSingletonPlutusVersion = SPlutusV2

instance AutoSingletonPlutusVersion 'PlutusV3 where
  autoSingletonPlutusVersion = SPlutusV3

-- | Type class for automatically deriving an 'SInstanceType' singleton.
class AutoSingletonInstanceType (it :: InstanceType) where
  -- | Method to produce the 'SInstanceType' singleton.
  autoSingletonInstanceType :: SInstanceType it

instance AutoSingletonInstanceType 'SingleInstance where
  autoSingletonInstanceType = SSingleInstance

instance AutoSingletonInstanceType 'MultiInstance where
  autoSingletonInstanceType = SMultiInstance

-- ============================================================================
-- 4. ACTION & OPERATION AUTO GENERATION
-- ============================================================================

-- | Type class for automatically deriving an 'SActionStepList' from a list of 'ActionStep's.
class AutoSingletonActionStepList (steps :: [ActionStep]) where
  -- | Method to produce the 'SActionStepList' singleton.
  autoSingletonActionStepList :: SActionStepList steps

instance AutoSingletonActionStepList '[] where
  autoSingletonActionStepList = ASSLNil

instance ( AutoSingletonActionStep step
         , AutoSingletonActionStepList rest
         ) => AutoSingletonActionStepList (step ': rest) where
  autoSingletonActionStepList = ASSLCons autoSingletonActionStep autoSingletonActionStepList

-- | Type class for automatically deriving an 'SActionStep' singleton.
class AutoSingletonActionStep (step :: ActionStep) where
  -- | Method to produce the 'SActionStep' singleton.
  autoSingletonActionStep :: SActionStep step

instance (AutoSingletonOperation op) => AutoSingletonActionStep ('Op op) where
  autoSingletonActionStep = SOp autoSingletonOperation

instance (KnownSymbol label, AutoSingletonOperation op) => AutoSingletonActionStep ('Let label op) where
  autoSingletonActionStep = SLet (Proxy @label) autoSingletonOperation

instance (KnownSymbol param, AutoSingletonOperation op, AutoSingletonCollectionConstraintList constraints) 
      => AutoSingletonActionStep ('Map op param constraints) where
  autoSingletonActionStep = SMap autoSingletonOperation (Proxy @param) autoSingletonCollectionConstraintList

-- | Type class for automatically deriving an 'SActionSpec' singleton.
class AutoSingletonActionSpec (spec :: TypedActionSpec) where
  -- | Method to produce the 'SActionSpec' singleton.
  autoSingletonActionSpec :: SActionSpec spec

instance ( KnownSymbol name
         , AutoSingletonActionStepList steps
         , AutoSingletonConstraintList constraints
         , AutoSingletonParamList params
         ) => AutoSingletonActionSpec ('ActionSpec name steps constraints params) where
  autoSingletonActionSpec = SActionSpec (Proxy @name) 
                                       (autoSingletonActionStepList @steps)
                                       (autoSingletonConstraintList @constraints)
                                       autoSingletonParamList

-- | Type class for automatically deriving an 'SActionSpecList'.
class AutoSingletonActionSpecs (specs :: [TypedActionSpec]) where
  -- | Method to produce the 'SActionSpecList' singleton.
  autoSingletonActionSpecs :: SActionSpecList specs

instance AutoSingletonActionSpecs '[] where
  autoSingletonActionSpecs = SASNil

instance ( AutoSingletonActionSpec spec
         , AutoSingletonActionSpecs rest
         ) => AutoSingletonActionSpecs (spec ': rest) where
  autoSingletonActionSpecs = SASCons autoSingletonActionSpec autoSingletonActionSpecs

-- | Type class for automatically deriving an 'SOperationList'.
class AutoSingletonOperationList (ops :: [TypedOperation]) where
  -- | Method to produce the 'SOperationList' singleton.
  autoSingletonOperationList :: SOperationList ops

instance AutoSingletonOperationList '[] where
  autoSingletonOperationList = SOLNil

instance ( AutoSingletonOperation op
         , AutoSingletonOperationList rest
         ) => AutoSingletonOperationList (op ': rest) where
  autoSingletonOperationList = SOLCons autoSingletonOperation autoSingletonOperationList

-- | Type class for automatically deriving an 'SOperation' singleton.
class AutoSingletonOperation (op :: TypedOperation) where
  -- | Method to produce the 'SOperation' singleton.
  autoSingletonOperation :: SOperation op

-- Instance for Create: Derives SStateSpec, SFieldSpecList, SConstraintList. Requires StateSpec, GBuildDatumFromSpecs.
instance ( StateSpec s
         , AutoSingletonStateSpec s
         , GBuildDatumFromSpecs (Rep (StateDatum s))
         , AutoSingletonFieldSpecList fields
         , AutoSingletonConstraintList constraints
         ) => AutoSingletonOperation ('Create @s fields constraints) where
  autoSingletonOperation = SCreate autoSingletonStateSpec autoSingletonFieldSpecList autoSingletonConstraintList

-- Instance for Update: Derives SStateRef, SFieldSpecList, SConstraintList. Requires StateSpec, GBuildDatumFromSpecs.
instance ( StateSpec s
         , GBuildDatumFromSpecs (Rep (StateDatum s))
         , AutoSingletonStateRef s ref
         , AutoSingletonFieldSpecList fields
         , AutoSingletonConstraintList constraints
         ) => AutoSingletonOperation ('Update @s ref fields constraints) where
  autoSingletonOperation = SUpdate autoSingletonStateRef autoSingletonFieldSpecList autoSingletonConstraintList

-- Instance for Delete: Derives SStateRef, SConstraintList. Requires StateSpec.
instance ( StateSpec s
         , AutoSingletonStateRef s ref
         , AutoSingletonConstraintList constraints
         ) => AutoSingletonOperation ('Delete @s ref constraints) where
  autoSingletonOperation = SDelete autoSingletonStateRef autoSingletonConstraintList

-- Instance for Reference: Derives SStateRef, SConstraintList. Requires StateSpec.
instance ( StateSpec s
         , AutoSingletonStateRef s ref
         , AutoSingletonConstraintList constraints
         ) => AutoSingletonOperation ('Reference @s ref constraints) where
  autoSingletonOperation = SReference autoSingletonStateRef autoSingletonConstraintList

-- ============================================================================
-- 5. FIELD & CONSTRAINT AUTO GENERATION
-- ============================================================================

-- | Type class for automatically deriving an 'SFieldSpecList'.
class AutoSingletonFieldSpecList (fields :: [FieldSpec]) where
  -- | Method to produce the 'SFieldSpecList' singleton.
  autoSingletonFieldSpecList :: SFieldSpecList fields

instance AutoSingletonFieldSpecList '[] where
  autoSingletonFieldSpecList = SFSNil

instance ( AutoSingletonFieldSpec field
         , AutoSingletonFieldSpecList rest
         ) => AutoSingletonFieldSpecList (field ': rest) where
  autoSingletonFieldSpecList = SFSCons autoSingletonFieldSpec autoSingletonFieldSpecList

-- | Type class for automatically deriving an 'SFieldSpec' singleton.
class AutoSingletonFieldSpec (field :: FieldSpec) where
  -- | Method to produce the 'SFieldSpec' singleton.
  autoSingletonFieldSpec :: SFieldSpec field

instance (KnownSymbol name, AutoSingletonTypedValue value) => AutoSingletonFieldSpec ('SetTo name value) where
  autoSingletonFieldSpec = SSetTo (Proxy @name) autoSingletonTypedValue

instance (KnownSymbol name) => AutoSingletonFieldSpec ('Preserve name) where
  autoSingletonFieldSpec = SPreserve (Proxy @name)

-- | Type class for automatically deriving an 'STypedValue' singleton.
class AutoSingletonTypedValue (value :: TypedValue) where
  -- | Method to produce the 'STypedValue' singleton.
  autoSingletonTypedValue :: STypedValue value

instance (KnownSymbol name) => AutoSingletonTypedValue ('ParamValue name) where
  autoSingletonTypedValue = SParamValue (Proxy @name)

instance (Typeable t, FromEnumValue t, KnownSymbol s, ToData t, Eq t) => AutoSingletonTypedValue ('EnumValue t s) where
  autoSingletonTypedValue = SEnumValue (Proxy @t) (Proxy @s)

instance (KnownNat n) => AutoSingletonTypedValue ('IntValue n) where
  autoSingletonTypedValue = SIntValue (Proxy @n)

instance (KnownSymbol label, KnownSymbol field) => AutoSingletonTypedValue ('StateFieldValue label field) where
  autoSingletonTypedValue = SStateFieldValue (Proxy @label) (Proxy @field)

instance AutoSingletonTypedValue 'CurrentTime where
  autoSingletonTypedValue = SCurrentTime

instance (AutoSingletonTypedValue v1, AutoSingletonTypedValue v2) => AutoSingletonTypedValue ('AddValue v1 v2) where
  autoSingletonTypedValue = SAddValue autoSingletonTypedValue autoSingletonTypedValue

instance (AutoSingletonTypedValue v1, AutoSingletonTypedValue v2) => AutoSingletonTypedValue ('SubtractValue v1 v2) where
  autoSingletonTypedValue = SSubtractValue autoSingletonTypedValue autoSingletonTypedValue

instance (AutoSingletonTypedValue v1, AutoSingletonTypedValue v2) => AutoSingletonTypedValue ('MultiplyValue v1 v2) where
  autoSingletonTypedValue = SMultiplyValue autoSingletonTypedValue autoSingletonTypedValue

instance (AutoSingletonTypedValue v1, AutoSingletonTypedValue v2) => AutoSingletonTypedValue ('DivideValue v1 v2) where
  autoSingletonTypedValue = SDivideValue autoSingletonTypedValue autoSingletonTypedValue

-- | Type class for automatically deriving an 'SConstraintList'. 
class AutoSingletonConstraintList (constraints :: [TypedConstraint]) where
  -- | Method to produce the 'SConstraintList' singleton.
  autoSingletonConstraintList :: SConstraintList constraints

instance AutoSingletonConstraintList '[] where
  autoSingletonConstraintList = SCLNil

instance ( AutoSingletonConstraint constraint
         , AutoSingletonConstraintList rest
         ) => AutoSingletonConstraintList (constraint ': rest) where
  autoSingletonConstraintList = SCLCons autoSingletonConstraint autoSingletonConstraintList

-- | Type class for automatically deriving an 'SConstraint' singleton.
class AutoSingletonConstraint (constraint :: TypedConstraint) where
  -- | Method to produce the 'SConstraint' singleton.
  autoSingletonConstraint :: SConstraint constraint

instance (KnownSymbol vName, KnownSymbol param) => AutoSingletonConstraint ('MustSpendValidatorParam vName param) where
  autoSingletonConstraint = SMustSpendValidatorParam (Proxy @vName) (Proxy @param)

instance (KnownSymbol param) => AutoSingletonConstraint ('MustSpendActionParam param) where
  autoSingletonConstraint = SMustSpendActionParam (Proxy @param)

-- | Instance for 'MustNotExist'. Uses explicit type application @s to avoid ambiguity.
instance (StateSpec s, AutoSingletonStateSpec s) => AutoSingletonConstraint ('MustNotExist ('TypedTheOnlyInstance @s)) where
  autoSingletonConstraint = SMustNotExist (STypedTheOnlyInstance (autoSingletonStateSpec @s))

-- | Instance for 'MustBeSignedByState' with 'TypedTheOnlyInstance'.
instance forall s field.
  ( KnownSymbol field
  , StateSpec s
  , AutoSingletonStateSpec s
  , GExtractField (Rep (StateDatum s))
  ) => AutoSingletonConstraint ('MustBeSignedByState ('TypedTheOnlyInstance @s) field) where
  autoSingletonConstraint = SMustBeSignedByState (STypedTheOnlyInstance (autoSingletonStateSpec @s)) (Proxy @field)

-- | Instance for 'MustBeSignedByState' with 'TypedUniqueWhere'.
instance forall s pred field.
  ( KnownSymbol field
  , StateSpec s
  , AutoSingletonStateSpec s
  , AutoSingletonPredicate pred
  , GExtractField (Rep (StateDatum s))
  ) => AutoSingletonConstraint ('MustBeSignedByState ('TypedUniqueWhere @s pred) field) where
  autoSingletonConstraint = SMustBeSignedByState (STypedUniqueWhere (autoSingletonStateSpec @s) autoSingletonPredicate) (Proxy @field)

instance (KnownSymbol param) => AutoSingletonConstraint ('MustBeSignedByParam param) where
  autoSingletonConstraint = SMustBeSignedByParam (Proxy @param)

instance ( StateSpec s
         , AutoSingletonStateSpec s
         , AutoSingletonTypedValue value
         ) => AutoSingletonConstraint ('MustAddToAggregateState s value) where
  autoSingletonConstraint = SMustAddToAggregateState autoSingletonStateSpec autoSingletonTypedValue

instance ( StateSpec s
         , AutoSingletonStateSpec s
         , AutoSingletonTypedValue value
         , AutoSingletonTypedValue address
         ) => AutoSingletonConstraint ('MustWithdrawFromAggregateState s value address) where
  autoSingletonConstraint = SMustWithdrawFromAggregateState autoSingletonStateSpec autoSingletonTypedValue autoSingletonTypedValue

-- | Type class for automatically deriving an 'SCollectionConstraintList'.
class AutoSingletonCollectionConstraintList (constraints :: [CollectionConstraint]) where
  -- | Method to produce the 'SCollectionConstraintList' singleton.
  autoSingletonCollectionConstraintList :: SCollectionConstraintList constraints

instance AutoSingletonCollectionConstraintList '[] where
  autoSingletonCollectionConstraintList = SCCNil

instance (AutoSingletonCollectionConstraint constraint, AutoSingletonCollectionConstraintList rest) 
      => AutoSingletonCollectionConstraintList (constraint ': rest) where
  autoSingletonCollectionConstraintList = SCCCons autoSingletonCollectionConstraint autoSingletonCollectionConstraintList

-- | Type class for automatically deriving an 'SCollectionConstraint' singleton.
class AutoSingletonCollectionConstraint (constraint :: CollectionConstraint) where
  -- | Method to produce the 'SCollectionConstraint' singleton.
  autoSingletonCollectionConstraint :: SCollectionConstraint constraint

instance (KnownSymbol field) => AutoSingletonCollectionConstraint ('MustHaveUniqueField field) where
  autoSingletonCollectionConstraint = SMustHaveUniqueField (Proxy @field)

-- ============================================================================
-- 6. STATE & REFERENCE AUTO GENERATION
-- ============================================================================

-- | Type class for automatically deriving an 'SStateList' from a type-level list of states.
class AutoSingletonStateList (states :: [Type]) where
  -- | Method to produce the 'SStateList' singleton.
  autoSingletonStateList :: SStateList states

-- | Base case: An empty list of states maps to 'SSNil'.
instance AutoSingletonStateList '[] where
  autoSingletonStateList = SSNil

-- | Recursive case: Derives 'SStateSpec' for the head and combines it with the tail.
instance ( StateSpec s
         , AutoSingletonStateSpec s
         , AutoSingletonStateList rest
         ) => AutoSingletonStateList (s ': rest) where
  autoSingletonStateList = SSCons autoSingletonStateSpec autoSingletonStateList

-- | Type class for automatically deriving an 'SStateRef' singleton.
class AutoSingletonStateRef (s :: Type) (ref :: TypedStateRef s) where
  autoSingletonStateRef :: SStateRef s ref

instance (StateSpec s, AutoSingletonStateSpec s) => AutoSingletonStateRef s 'TypedTheOnlyInstance where
  autoSingletonStateRef = STypedTheOnlyInstance autoSingletonStateSpec

instance (StateSpec s, AutoSingletonStateSpec s, AutoSingletonPredicate pred) => AutoSingletonStateRef s ('TypedUniqueWhere pred) where
  autoSingletonStateRef = STypedUniqueWhere autoSingletonStateSpec autoSingletonPredicate

instance (StateSpec s, AutoSingletonStateSpec s) => AutoSingletonStateRef s 'TypedAny where
  autoSingletonStateRef = STypedAny autoSingletonStateSpec

instance (StateSpec s, AutoSingletonStateSpec s, AutoSingletonPredicate pred) => AutoSingletonStateRef s ('TypedAnyWhere pred) where
  autoSingletonStateRef = STypedAnyWhere autoSingletonStateSpec autoSingletonPredicate

instance (StateSpec s, AutoSingletonStateSpec s, KnownSymbol label) => AutoSingletonStateRef s ('TypedByLabel label) where
  autoSingletonStateRef = STypedByLabel autoSingletonStateSpec (Proxy @label)

-- | Type class for automatically deriving an 'SPredicate' singleton.
class AutoSingletonPredicate (pred :: TypedPredicate st) where
  -- | Method to produce the 'SPredicate' singleton.
  autoSingletonPredicate :: SPredicate pred

instance
  ( KnownSymbol field
  , AutoSingletonTypedValue value
  ) => AutoSingletonPredicate ('FieldEquals field value) where
  autoSingletonPredicate = SFieldEquals (Proxy @field) autoSingletonTypedValue

-- | Instance for 'And' predicate.
instance (AutoSingletonPredicate p1, AutoSingletonPredicate p2) => AutoSingletonPredicate ('And p1 p2) where
  autoSingletonPredicate = SAnd autoSingletonPredicate autoSingletonPredicate

-- ============================================================================
-- 7. PARAMETER & DERIVATION AUTO GENERATION
-- ============================================================================

-- | Type class for automatically deriving an 'SParamList'.
class AutoSingletonParamList (params :: [(Symbol, Type)]) where
  -- | Method to produce the 'SParamList' singleton.
  autoSingletonParamList :: SParamList params

instance AutoSingletonParamList '[] where
  autoSingletonParamList = SPNil

instance ( KnownSymbol name, Typeable t, AutoSingletonParamList rest
         ) => AutoSingletonParamList ('(name, t) ': rest) where
  autoSingletonParamList = SPCons (Proxy @name) (Proxy @t) (autoSingletonParamList @rest)

-- | Type class for automatically deriving an 'SAppInstanceParamList'.
class AutoSingletonAppInstanceParams (params :: [(Symbol, Symbol)]) where
  -- | Method to produce the 'SAppInstanceParamList' singleton.
  autoSingletonAppInstanceParams :: SAppInstanceParamList params

instance AutoSingletonAppInstanceParams '[] where
  autoSingletonAppInstanceParams = SAPNil

instance ( KnownSymbol vName
         , KnownSymbol param
         , AutoSingletonAppInstanceParams rest
         ) => AutoSingletonAppInstanceParams ('(vName, param) ': rest) where
  autoSingletonAppInstanceParams = SAPCons (Proxy @vName) (Proxy @param) (autoSingletonAppInstanceParams @rest)

-- | Type class for automatically deriving an 'SParamDerivationList'.
class AutoSingletonParamDerivationList (derivations :: [ParamDerivation]) where
  -- | Method to produce the 'SParamDerivationList' singleton.
  autoSingletonParamDerivationList :: SParamDerivationList derivations

instance AutoSingletonParamDerivationList '[] where
  autoSingletonParamDerivationList = SPDLNil

instance ( AutoSingletonParamDerivation derivation
         , AutoSingletonParamDerivationList rest
         ) => AutoSingletonParamDerivationList (derivation ': rest) where
  autoSingletonParamDerivationList = SPDLCons autoSingletonParamDerivation autoSingletonParamDerivationList

-- | Type class for automatically deriving an 'SParamDerivation' singleton.
class AutoSingletonParamDerivation (derivation :: ParamDerivation) where
  -- | Method to produce the 'SParamDerivation' singleton.
  autoSingletonParamDerivation :: SParamDerivation derivation

instance ( KnownSymbol paramName
         , AutoSingletonDerivationSource source
         ) => AutoSingletonParamDerivation ('DeriveParam paramName (source :: DerivationSource t)) where
  autoSingletonParamDerivation = SDeriveParam (Proxy @paramName) autoSingletonDerivationSource
  
-- | Type class for automatically deriving an 'SDerivationSource' singleton.
class AutoSingletonDerivationSource (source :: DerivationSource t) where
  -- | Method to produce the 'SDerivationSource' singleton.
  autoSingletonDerivationSource :: SDerivationSource source

instance (KnownSymbol validator) => AutoSingletonDerivationSource ('ValidatorAddress validator) where
  autoSingletonDerivationSource = SValidatorAddress (Proxy @validator)

instance (KnownSymbol validator) => AutoSingletonDerivationSource ('ValidatorHash validator) where
  autoSingletonDerivationSource = SValidatorHash (Proxy @validator)

-- ============================================================================
-- 8. STATE PROPERTY AUTO GENERATION
-- ============================================================================

-- | Automatically derives the 'SPolicySource' singleton from its type-level specification.
class AutoSingletonPolicySource (ps :: SpecPolicySource) where
  -- | Method to produce the 'SPolicySource' singleton.
  autoSingletonPolicySource :: SPolicySource ps

-- | Instance for 'OwnPolicySpec'.
instance AutoSingletonPolicySource 'OwnPolicySpec where
  autoSingletonPolicySource = SOwnPolicy

-- | Instance for 'ExternalPolicySpec'.
instance (KnownSymbol sym) => AutoSingletonPolicySource ('ExternalPolicySpec sym) where
  autoSingletonPolicySource = SExternalPolicy (Proxy @sym)

-- | Automatically derives the 'SStateIdentifier' singleton from its type-level specification.
class AutoSingletonStateIdentifier (si :: SpecStateIdentifier) where
  -- | Method to produce the 'SStateIdentifier' singleton.
  autoSingletonStateIdentifier :: SStateIdentifier si

-- | Instance for 'TokenIdentifiedSpec'.
instance (AutoSingletonPolicySource p, KnownSymbol tn, KnownNat q)
      => AutoSingletonStateIdentifier ('TokenIdentifiedSpec p tn q) where
  autoSingletonStateIdentifier = STokenIdentified autoSingletonPolicySource (Proxy @tn) (Proxy @q)

-- | Instance for 'AggregateAssetSpec'.
instance (AutoSingletonPolicySource p, KnownSymbol tn) => AutoSingletonStateIdentifier ('AggregateAssetSpec p tn) where
  autoSingletonStateIdentifier = SAggregateAsset autoSingletonPolicySource (Proxy @tn)

-- | Automatically derives the 'SDatumField' singleton.
class AutoSingletonDatumField (field :: (Symbol, Type)) where
  autoSingletonDatumField :: SDatumField field

-- | Instance for a standard field definition.
instance (KnownSymbol name, Typeable t) => AutoSingletonDatumField '(name, t) where
  autoSingletonDatumField = SDatumField (Proxy @name) (Proxy @t)

-- | Automatically derives the 'SDatumFieldList' singleton.
class AutoSingletonDatumFieldList (fields :: [(Symbol, Type)]) where
  autoSingletonDatumFieldList :: SDatumFieldList fields

-- | Base case: empty list.
instance AutoSingletonDatumFieldList '[] where
  autoSingletonDatumFieldList = SDFNil

-- | Recursive step: head and tail.
instance (AutoSingletonDatumField field, AutoSingletonDatumFieldList rest)
      => AutoSingletonDatumFieldList (field ': rest) where
  autoSingletonDatumFieldList = SDFCons autoSingletonDatumField autoSingletonDatumFieldList

-- | Automatically derives the 'SRefStrategy' singleton.
class AutoSingletonRefStrategy (rs :: RefStrategy) where
  autoSingletonRefStrategy :: SRefStrategy rs

-- | Instance for 'OnlyByProperty'.
instance AutoSingletonRefStrategy 'OnlyByProperty where
  autoSingletonRefStrategy = SOnlyByProperty

-- | Instance for 'OnlyAsUnique'.
instance AutoSingletonRefStrategy 'OnlyAsUnique where
  autoSingletonRefStrategy = SOnlyAsUnique

-- | Instance for 'AnyRef'.
instance AutoSingletonRefStrategy 'AnyRef where
  autoSingletonRefStrategy = SAnyRef

-- | Instance for 'NoRef'.
instance AutoSingletonRefStrategy 'NoRef where
  autoSingletonRefStrategy = SNoRef

-- | Type class for automatically deriving 'SMappability' singletons based on the 'Bool' index.
class AutoSingletonMappability (m :: Bool) where
  -- | Returns the corresponding 'SMappability' singleton.
  autoSingletonMappability :: SMappability m

-- | Instance for 'True' yielding 'SMappable'.
instance AutoSingletonMappability 'True where
  autoSingletonMappability = SMappable

-- | Instance for 'False' yielding 'SNotMappable'.
instance AutoSingletonMappability 'False where
  autoSingletonMappability = SNotMappable

-- | Automatically derives the full 'SStateSpec' singleton for a state 's'.
class AutoSingletonStateSpec (s :: Type) where
  autoSingletonStateSpec :: SStateSpec s

-- | Standard instance for any type 's' with a 'StateSpec' instance.
-- Recursively derives singletons for fields, identifier, and strategy.
instance ( StateSpec s
         , AutoSingletonDatumFieldList (DatumFields s)
         , AutoSingletonStateIdentifier (Identifier s)
         , AutoSingletonRefStrategy (Strategy s)
         , AutoSingletonMappability (HasMappable s)
         ) => AutoSingletonStateSpec s where
  autoSingletonStateSpec = SStateSpec
    autoSingletonDatumFieldList
    autoSingletonStateIdentifier
    autoSingletonRefStrategy
    autoSingletonMappability