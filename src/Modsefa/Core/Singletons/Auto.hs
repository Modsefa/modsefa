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
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable)
import GHC.Generics (Generic, Rep)
import GHC.TypeLits (KnownNat, KnownSymbol, Symbol)

import GeniusYield.Types (PlutusVersion(..))
import PlutusLedgerApi.V3 (ToData)

import Modsefa.Core.Foundation
  ( ActionStep(..), AppSpec(..), CollectionConstraint(..), DerivationSource(..)
  , FieldSpec(..), GExtractField, GetStateData, InstanceType(..)
  , ParamDerivation(..), ParamsToValue, SStateType(SStateType), StateRepresentable
  , StateType(..), TypedActionSpec(..), TypedConstraint(..), TypedOperation(..)
  , TypedPredicate(And, FieldEquals), TypedStateRef(..), TypedValue(..)
  , ValidatorDef(..), ValidatorSpec(..)
  )
  
import Modsefa.Core.Singletons.Types
  ( FromEnumValue, GBuildDatumFromSpecs, SActionSpec(..), SActionSpecList(..)
  , SActionStep(..), SActionStepList(..), SActionTransition(..)
  , SActionTransitionList(..), SAppInstanceParamList(..), SAppSpec(..)
  , SAppStateList(..), SCollectionConstraint(..), SCollectionConstraintList(..)
  , SConstraint(..), SConstraintList(..), SDerivationSource(..), SFieldSpec(..)
  , SFieldSpecList(..), SInitialAppState(..), SInstanceType(..), SOperation(..)
  , SOperationList(..), SParamDerivation(..), SParamDerivationList(..)
  , SParamList(..), SPlutusVersion(..), SPredicate(..), SStateList(..)
  , SStateRef(..), STypedValue(..), SValidator(..), SValidatorList(..)
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
class AutoSingletonActionTransitionList (transitions :: [(TypedActionSpec app, Symbol, Symbol)]) where
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
class AutoSingletonActionTransition (transition :: (TypedActionSpec app, Symbol, Symbol)) where
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
class AutoSingletonActionSpec (spec :: TypedActionSpec app) where
  -- | Method to produce the 'SActionSpec' singleton.
  autoSingletonActionSpec :: SActionSpec app spec

instance ( KnownSymbol name
         , AutoSingletonActionStepList steps
         , AutoSingletonConstraintList constraints
         , AutoSingletonParamList params
         ) => AutoSingletonActionSpec ('ActionSpec @app name steps constraints params) where
  autoSingletonActionSpec = SActionSpec (Proxy @name) 
                                       (autoSingletonActionStepList @steps)
                                       (autoSingletonConstraintList @constraints)
                                       autoSingletonParamList

-- | Type class for automatically deriving an 'SActionSpecList'.
class AutoSingletonActionSpecs (specs :: [TypedActionSpec app]) where
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

-- Instance for Create: Derives SStateType, SFieldSpecList, SConstraintList. Requires StateRepresentable, GBuildDatumFromSpecs.
instance ( st ~ 'ST name record -- Ensures st matches the pattern
         , KnownSymbol name
         , StateRepresentable st
         , GBuildDatumFromSpecs (Rep record)
         , AutoSingletonFieldSpecList fields
         , AutoSingletonConstraintList constraints
         ) => AutoSingletonOperation ('Create @('ST name record) fields constraints) where
  autoSingletonOperation = SCreate (SStateType @st) autoSingletonFieldSpecList autoSingletonConstraintList

-- Instance for Update: Derives SStateRef, SFieldSpecList, SConstraintList. Requires StateRepresentable, GBuildDatumFromSpecs.
instance ( StateRepresentable st
         , GBuildDatumFromSpecs (Rep (GetStateData st))
         , AutoSingletonStateRef st ref
         , AutoSingletonFieldSpecList fields
         , AutoSingletonConstraintList constraints
         ) => AutoSingletonOperation ('Update @st ref fields constraints) where
  autoSingletonOperation = SUpdate autoSingletonStateRef autoSingletonFieldSpecList autoSingletonConstraintList

-- Instance for Delete: Derives SStateRef, SConstraintList. Requires StateRepresentable.
instance ( StateRepresentable st
         , AutoSingletonStateRef st ref
         , AutoSingletonConstraintList constraints
         ) => AutoSingletonOperation ('Delete @st ref constraints) where
  autoSingletonOperation = SDelete autoSingletonStateRef autoSingletonConstraintList

-- Instance for Reference: Derives SStateRef, SConstraintList. Requires StateRepresentable.
instance ( StateRepresentable st
         , AutoSingletonStateRef st ref
         , AutoSingletonConstraintList constraints
         ) => AutoSingletonOperation ('Reference @st ref constraints) where
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

instance (st ~ 'ST name record, KnownSymbol name, StateRepresentable st) => AutoSingletonConstraint ('MustNotExist @st 'TypedTheOnlyInstance) where
  autoSingletonConstraint = SMustNotExist (STypedTheOnlyInstance (SStateType @st))

instance forall st field.
  ( KnownSymbol field
  , StateRepresentable st
  , Generic (GetStateData st)
  , GExtractField (Rep (GetStateData st))
  ) => AutoSingletonConstraint ('MustBeSignedByState ('TypedTheOnlyInstance @st) field) where
  autoSingletonConstraint = SMustBeSignedByState (STypedTheOnlyInstance (SStateType @st)) (Proxy @field)

instance forall st pred field.
  ( KnownSymbol field
  , AutoSingletonPredicate pred
  , StateRepresentable st
  , Generic (GetStateData st)
  , GExtractField (Rep (GetStateData st))
  ) => AutoSingletonConstraint ('MustBeSignedByState ('TypedUniqueWhere @st pred) field) where
  autoSingletonConstraint = SMustBeSignedByState (STypedUniqueWhere (SStateType @st) autoSingletonPredicate) (Proxy @field)

instance (KnownSymbol param) => AutoSingletonConstraint ('MustBeSignedByParam param) where
  autoSingletonConstraint = SMustBeSignedByParam (Proxy @param)

instance (StateRepresentable st, AutoSingletonTypedValue value) => AutoSingletonConstraint ('MustAddToAggregateState st value) where
  autoSingletonConstraint = SMustAddToAggregateState (SStateType @st) autoSingletonTypedValue

instance (StateRepresentable st, AutoSingletonTypedValue value, AutoSingletonTypedValue address) => AutoSingletonConstraint ('MustWithdrawFromAggregateState st value address) where
  autoSingletonConstraint = SMustWithdrawFromAggregateState (SStateType @st) autoSingletonTypedValue autoSingletonTypedValue

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

-- | Type class for automatically deriving an 'SStateList'.
class AutoSingletonStateList (states :: [StateType]) where
  -- | Method to produce the 'SStateList' singleton.
  autoSingletonStateList :: SStateList states

instance AutoSingletonStateList '[] where
  autoSingletonStateList = SSNil

instance ( StateRepresentable st
         , AutoSingletonStateList rest
         ) => AutoSingletonStateList (st ': rest) where
  autoSingletonStateList = SSCons (SStateType @st) (autoSingletonStateList @rest)

-- | Type class for automatically deriving an 'SStateRef' singleton.
class AutoSingletonStateRef (st :: StateType) (ref :: TypedStateRef st) where
  -- | Method to produce the 'SStateRef' singleton.
  autoSingletonStateRef :: SStateRef st ref

instance (StateRepresentable st) => AutoSingletonStateRef st 'TypedTheOnlyInstance where
  autoSingletonStateRef = STypedTheOnlyInstance (SStateType @st)

instance (AutoSingletonPredicate pred, StateRepresentable st) => AutoSingletonStateRef st ('TypedUniqueWhere pred) where
  autoSingletonStateRef = STypedUniqueWhere (SStateType @st) autoSingletonPredicate

instance (StateRepresentable st) => AutoSingletonStateRef st 'TypedAny where
  autoSingletonStateRef = STypedAny (SStateType @st)

instance (AutoSingletonPredicate pred, StateRepresentable st) => AutoSingletonStateRef st ('TypedAnyWhere pred) where
  autoSingletonStateRef = STypedAnyWhere (SStateType @st) autoSingletonPredicate

instance (KnownSymbol label, StateRepresentable st) => AutoSingletonStateRef st ('TypedByLabel label) where
  autoSingletonStateRef = STypedByLabel (SStateType @st) (Proxy @label)

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