{-|
Module      : Modsefa.Core.Foundation.TypeFamilies
Description : Core type families for Modsefa DSL.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC (requires TypeFamilies, PolyKinds, UndecidableInstances)

This module consolidates all type-level computation and extraction
functions (type families) used throughout the Modsefa library. These are essential
for enabling type-safe specification analysis, validation, and code generation.
Type families are organized by their primary domain of operation (state types,
actions, parameters, etc.).
-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type families for the Modsefa library
--
-- This module consolidates all type-level computation and extraction
-- functions used throughout the library. Type families are organized
-- by their primary domain of operation.
--
-- Organization:
--   1. State Type Analysis
--   2. Action Specification Analysis
--   3. Collection Operations & Utilities
--   4. Parameter Type Resolution
--   5. Validation Type Families
--   6. Predicate Type Families
module Modsefa.Core.Foundation.TypeFamilies
  ( -- * State Type Analysis
    ExtractStateType
    
    -- * Action Analysis
  , ActionSpecName
  , ActionSpecSteps
  , ActionSpecConstraints
  , ActionSpecParameters
    
    -- * Collection Operations
  , ExtractStateTypes
  , ExtractOpsFromAction
  , ExtractConstraintsFromAction
  , ExtractOpsFromActionSteps
    
    -- * Parameter Resolution
  , ParamsToValue
  , ResolveInstanceParamList
  , LookupValidatorParamType
    
    -- * Validation Helpers
  , AllStateTypesGeneric
  , ExtractPlutusVersion
  , ExtractPlutusVersionFromValidators
  ) where

import Data.Kind (Constraint, Type)
import Data.Type.Bool (If)
import Data.Type.Equality (type (==))
import GHC.Generics (Generic, K1, M1, Meta(MetaSel), S, (:*:))
import GHC.TypeLits (ErrorMessage(..), Symbol, TypeError)

import GeniusYield.Types (PlutusVersion)

import Modsefa.Core.Foundation.Types
  ( ActionStep(..), AppSpec(Validators), GetStateData, StateType(..)
  , TypedActionSpec(..), TypedConstraint, TypedOperation(..), TypedStateRef
  , ValidatorDef(..), ValidatorSpec(..)
  )


-- ============================================================================
-- 1. STATE TYPE ANALYSIS
-- ============================================================================

-- | Extracts the target 'StateType' from a 'TypedOperation'.
-- Useful for determining which state an operation acts upon.
type family ExtractStateType (op :: TypedOperation) :: StateType where
  ExtractStateType ('Create @(ST name record) _ _) = ST name record
  ExtractStateType ('Update ref _ _) = ExtractStateTypeFromRef ref
  ExtractStateType ('Delete ref _) = ExtractStateTypeFromRef ref  
  ExtractStateType ('Reference ref _) = ExtractStateTypeFromRef ref

-- | Extracts the 'StateType' associated with a 'TypedStateRef'.
type family ExtractStateTypeFromRef (ref :: TypedStateRef st) :: StateType where
  ExtractStateTypeFromRef (ref :: TypedStateRef st) = st

-- ============================================================================
-- 2. ACTION SPECIFICATION ANALYSIS
-- ============================================================================

-- | Extracts the action name ('Symbol') from a 'TypedActionSpec'.
type family ActionSpecName (spec :: TypedActionSpec app) :: Symbol where
  ActionSpecName ('ActionSpec name _ _ _) = name

-- | Extracts the list of 'ActionStep's from a 'TypedActionSpec'.
type family ActionSpecSteps (spec :: TypedActionSpec app) :: [ActionStep] where
  ActionSpecSteps ('ActionSpec _ steps _ _) = steps

-- | Extracts the list of 'TypedConstraint's from a 'TypedActionSpec'.
type family ActionSpecConstraints (spec :: TypedActionSpec app) :: [TypedConstraint] where
  ActionSpecConstraints ('ActionSpec _ _ constraints _) = constraints

-- | Extracts the parameter list (@[(Symbol, Type)]@) from a 'TypedActionSpec'.
type family ActionSpecParameters (spec :: TypedActionSpec app) :: [(Symbol, Type)] where
  ActionSpecParameters ('ActionSpec _ _ _ params) = params

-- ============================================================================
-- 3. COLLECTION OPERATIONS & UTILITIES
-- ============================================================================

-- | Type-level list concatenation, specifically for 'StateType' lists.
type family ConcatStateLists (states1 :: [StateType]) (states2 :: [StateType]) :: [StateType] where
  ConcatStateLists '[] states2 = states2
  ConcatStateLists (st ': rest) states2 = st ': ConcatStateLists rest states2

-- | Collects all 'ManagedStates' from a list of 'ValidatorDef's defined in an 'AppSpec'.
type family AllManagedStates (validators :: [ValidatorDef]) :: [StateType] where
  AllManagedStates '[] = '[]
  AllManagedStates ('Validator v ': rest) = ConcatStateLists (ManagedStates v) (AllManagedStates rest)

-- | Extracts all 'TypedOperation's embedded within a list of 'ActionStep's.
type family ExtractOpsFromActionSteps (steps :: [ActionStep]) :: [TypedOperation] where
  ExtractOpsFromActionSteps '[] = '[]
  ExtractOpsFromActionSteps ('Op op ': rest) = op ': ExtractOpsFromActionSteps rest
  ExtractOpsFromActionSteps ('Let _ op ': rest) = op ': ExtractOpsFromActionSteps rest
  ExtractOpsFromActionSteps ('Map op _ _ ': rest) = op ': ExtractOpsFromActionSteps rest

-- | Extracts the list of 'TypedActionSpec's from an application's 'ActionTransitions'.
type family ExtractActionSpecs (transitions :: [(TypedActionSpec app, Symbol, Symbol)]) :: [TypedActionSpec app] where
  ExtractActionSpecs '[] = '[]
  ExtractActionSpecs ('(spec, _, _) ': rest) = spec ': ExtractActionSpecs rest

-- | Extracts the target 'StateType' for each operation in a list of 'TypedOperation's.
type family ExtractStateTypes (ops :: [TypedOperation]) :: [StateType] where
  ExtractStateTypes '[] = '[]
  ExtractStateTypes (op ': rest) = ExtractStateType op ': ExtractStateTypes rest

-- | Extracts all 'TypedOperation's contained within a single 'TypedActionSpec'.
type family ExtractOpsFromAction (action :: TypedActionSpec app) :: [TypedOperation] where
  ExtractOpsFromAction ('ActionSpec _ steps _ _) = ExtractOpsFromActionSteps steps

-- | Extracts all top-level 'TypedConstraint's from a single 'TypedActionSpec'.
type family ExtractConstraintsFromAction (action :: TypedActionSpec app) :: [TypedConstraint] where
  ExtractConstraintsFromAction ('ActionSpec _ _ constraints _) = constraints

-- ============================================================================
-- 4. PARAMETER TYPE RESOLUTION
-- ============================================================================

-- | Converts a type-level parameter list (@[(Symbol, Type)]@) into the concrete
-- Haskell 'Type' used to hold the parameter values at runtime.
--
-- Examples:
--   @ParamsToValue '[] = ()@
--   @ParamsToValue '[ '("amount", Integer) ] = Integer@
--   @ParamsToValue '[ '("amount", Integer), '("recipient", PubKeyHash) ] = (Integer, PubKeyHash)@
type family ParamsToValue (params :: [(Symbol, Type)]) :: Type where
  ParamsToValue '[] = ()
  ParamsToValue '[ '(_, t) ] = t
  ParamsToValue ('(_, t) ': rest) = (t, ParamsToValue rest)

-- | Resolves the 'AppInstanceParameters' (which link validator names to parameter names)
-- into a concrete parameter list (@[(Symbol, Type)]@) by looking up the actual
-- types from the corresponding 'ValidatorSpec' definitions within the 'AppSpec'.
type family ResolveInstanceParamList (instanceParams :: [(Symbol, Symbol)]) (validators :: [ValidatorDef]) :: [(Symbol, Type)] where
  ResolveInstanceParamList '[] _ = '[]
  ResolveInstanceParamList ('(validatorName, paramName) ': rest) validators = 
    LookupValidatorParam validatorName paramName validators ': ResolveInstanceParamList rest validators

-- | Helper for 'ResolveInstanceParamList': Finds the parameter specification ('(Symbol, Type)')
-- for a given parameter name within a specific validator definition.
type family LookupValidatorParam (validatorName :: Symbol) (paramName :: Symbol) (validators :: [ValidatorDef]) :: (Symbol, Type) where
  LookupValidatorParam validatorName paramName ('Validator v ': rest) =
    If (ValidatorAppName v == validatorName)
       (LookupParamSpec paramName (Params v))
       (LookupValidatorParam validatorName paramName rest)
  LookupValidatorParam validatorName paramName '[] =
    TypeError ('Text "Validator " ':<>: 'ShowType validatorName ':<>: 'Text " not found")

-- | Helper for 'LookupValidatorParam': Finds the specification ('(Symbol, Type)') for a
-- parameter name within a validator's 'Params' list.
type family LookupParamSpec (paramName :: Symbol) (params :: [(Symbol, Type)]) :: (Symbol, Type) where
  LookupParamSpec paramName ('(name, t) ': rest) =
    If (name == paramName) 
       '(name, t) 
       (LookupParamSpec paramName rest)
  LookupParamSpec paramName '[] =
    TypeError ('Text "Parameter " ':<>: 'ShowType paramName ':<>: 'Text " not found")

-- | Looks up the 'Type' of a specific parameter ('Symbol') within a parameter list (@[(Symbol, Type)]@).
type family LookupParamInList (paramName :: Symbol) (params :: [(Symbol, Type)]) :: Type where
  LookupParamInList paramName ('(name, t) ': rest) =
    If (name == paramName) t (LookupParamInList paramName rest)
  LookupParamInList paramName '[] =
    TypeError ('Text "Parameter " ':<>: 'ShowType paramName ':<>: 'Text " not found")

-- | Looks up the 'Type' of a specific parameter ('param') within a specific validator ('vName')
-- defined in the application ('app'). Used for type validation.
type family LookupValidatorParamType (vName :: Symbol) (param :: Symbol) (app :: Type) :: Type where   
  LookupValidatorParamType vName param app = FindParamInValidatorList vName param (Validators app)

-- | Helper for 'LookupValidatorParamType': Searches the list of 'ValidatorDef's for the
-- specified validator and parameter.
type family FindParamInValidatorList (vName :: Symbol) (param :: Symbol) (validators :: [ValidatorDef]) :: Type where   
  FindParamInValidatorList vName param ('Validator v ': rest) =     
    If (ValidatorAppName v == vName)        
       (LookupParamInList param (Params v))        
       (FindParamInValidatorList vName param rest)
  FindParamInValidatorList vName param '[] =
    TypeError ('Text "Validator " ':<>: 'ShowType vName ':<>: 'Text " not found")

-- ============================================================================
-- 5. VALIDATION TYPE FAMILIES
-- ============================================================================

-- Note: The core validation type families (InitialStateInStates,
-- ValidateAppInstanceParameters, etc.) are declared in Foundation.Types
-- since they're used in the AppSpec superclass constraints.

-- | Constraint requiring that all 'StateType's in a list have 'Generic' instances
-- (needed for serialization and generic operations like field extraction).
type family AllStateTypesGeneric (states :: [StateType]) :: Constraint where
  AllStateTypesGeneric '[] = ()
  AllStateTypesGeneric (st ': rest) = 
    ( Generic (GetStateData st)
    , AllStateTypesGeneric rest
    )

-- | Extracts the 'PlutusVersion' used by the application.
-- Assumes all validators in the app use the same version.
type family ExtractPlutusVersion (app :: Type) :: PlutusVersion where
  ExtractPlutusVersion app = ExtractPlutusVersionFromValidators (Validators app)

-- | Helper for 'ExtractPlutusVersion': Extracts the 'PlutusVersion' from the first validator in the list.
type family ExtractPlutusVersionFromValidators (validators :: [ValidatorDef]) :: PlutusVersion where
  ExtractPlutusVersionFromValidators ('Validator v ': _) = ValidatorPlutusVersion v
  ExtractPlutusVersionFromValidators '[] = 
    TypeError ('Text "Cannot extract Plutus version from empty validator list")

-- | Checks if a given 'StateType' exists within a list of 'StateType's. Returns 'Bool'.
type family StateInStateList (st :: StateType) (states :: [StateType]) :: Bool where
  StateInStateList st (st ': _) = 'True
  StateInStateList st (_ ': rest) = StateInStateList st rest
  StateInStateList _ '[] = 'False

-- ============================================================================
-- 6. PREDICATE TYPE FAMILIES
-- ============================================================================

-- | Internal helper: Attempts the first type computation; falls back to the second if the first fails.
-- Used by 'GGetFieldType' to search both sides of a product type (:*:).
type family Try (a :: k) (b :: k) :: k where
  Try a _ = a

-- | Internal helper: Extracts the type of a field ('Symbol') from a record's
-- generic representation ('Rep'). Used for predicate validation.
type family GGetFieldType (rep :: Type -> Type) (field :: Symbol) :: Type where
  GGetFieldType (M1 S ('MetaSel ('Just field) _ _ _) (K1 _ fieldType)) field = fieldType
  GGetFieldType (M1 _ _ f) field = GGetFieldType f field
  GGetFieldType (f :*: g) field =
    Try (GGetFieldType f field) (Try (GGetFieldType g field) (TypeError ('Text "Field not found: " ':<>: 'ShowType field)))
