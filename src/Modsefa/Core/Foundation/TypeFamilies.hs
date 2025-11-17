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
module Modsefa.Core.Foundation.TypeFamilies
  ( -- * State Type Analysis
    ExtractStateType
  , ExtractStateFromRef
  , ExtractStateFromConstraint
    
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
  , FindParamInValidatorList
  , LookupParamInList
    
    -- * Validation Helpers
  , AllStateTypesGeneric
  , AllManagedStates
  , ExtractPlutusVersionFromValidators
  , MaybeStateDatumConstraints
  ) where

import Data.Kind (Constraint, Type)
import Data.Type.Bool (If)
import Data.Type.Equality (type (==))
import GHC.Generics (Generic)
import GHC.TypeLits (ErrorMessage(..), Symbol, TypeError)

import GeniusYield.Types (PlutusVersion)

import Modsefa.Core.Foundation.Types
  ( ActionStep(..), StateDatum, StateDatumConstraints, TypedActionSpec(..)
  , TypedConstraint(..), TypedOperation(..), TypedStateRef(..)
  )
import Modsefa.Core.Foundation.Validator (ValidatorDef(..), ValidatorSpec(..))


-- ============================================================================
-- 1. STATE TYPE ANALYSIS
-- ============================================================================

-- | Extracts the target state tag ('Type') from a 'TypedOperation'.
type family ExtractStateType (op :: TypedOperation) :: Type where
  ExtractStateType ('Create @s _ _) = s
  ExtractStateType ('Update ref _ _) = ExtractStateFromRef ref
  ExtractStateType ('Delete ref _) = ExtractStateFromRef ref  
  ExtractStateType ('Reference ref _) = ExtractStateFromRef ref

-- | Type family to extract the state type 's' from a 'TypedStateRef s'.
type family ExtractStateFromRef (ref :: TypedStateRef s) :: Type where
  ExtractStateFromRef (ref :: TypedStateRef s) = s

-- | Type family to safely extract the state type ('Maybe Type')
-- | referenced by a 'TypedConstraint'.
type family ExtractStateFromConstraint (c :: TypedConstraint) :: Maybe Type where
  -- Constraints *with* a state
  ExtractStateFromConstraint ('MustBeSignedByState ref _)     = 'Just (ExtractStateFromRef ref)
  ExtractStateFromConstraint ('PreserveStateField ref _)     = 'Just (ExtractStateFromRef ref)
  ExtractStateFromConstraint ('RequireStateValue ref _ _)   = 'Just (ExtractStateFromRef ref)
  ExtractStateFromConstraint ('MustExist ref)                 = 'Just (ExtractStateFromRef ref)
  ExtractStateFromConstraint ('MustNotExist ref)               = 'Just (ExtractStateFromRef ref)
  ExtractStateFromConstraint ('ExactlyN _ ref)                = 'Just (ExtractStateFromRef ref)
  ExtractStateFromConstraint ('AtLeastN _ ref)                = 'Just (ExtractStateFromRef ref)
  ExtractStateFromConstraint ('AtMostN _ ref)                = 'Just (ExtractStateFromRef ref)
  
  -- Constraints *without* a state (or with states handled differently)
  ExtractStateFromConstraint ('MustSpendParam _)                = 'Nothing
  ExtractStateFromConstraint ('MustBeSignedByParam _)           = 'Nothing
  ExtractStateFromConstraint ('MustSpendActionParam _)          = 'Nothing
  ExtractStateFromConstraint ('MustSpendValidatorParam _ _)     = 'Nothing
  ExtractStateFromConstraint ('MustBeSignedByValidatorParam _ _) = 'Nothing
  ExtractStateFromConstraint ('MustAddToAggregateState _ _)     = 'Nothing
  ExtractStateFromConstraint ('MustWithdrawFromAggregateState _ _ _) = 'Nothing

-- ============================================================================
-- 2. ACTION SPECIFICATION ANALYSIS
-- ============================================================================

-- | Extracts the action name ('Symbol') from a 'TypedActionSpec'.
type family ActionSpecName (spec :: TypedActionSpec) :: Symbol where
  ActionSpecName ('ActionSpec name _ _ _) = name

-- | Extracts the list of 'ActionStep's from a 'TypedActionSpec'.
type family ActionSpecSteps (spec :: TypedActionSpec) :: [ActionStep] where
  ActionSpecSteps ('ActionSpec _ steps _ _) = steps

-- | Extracts the list of 'TypedConstraint's from a 'TypedActionSpec'.
type family ActionSpecConstraints (spec :: TypedActionSpec) :: [TypedConstraint] where
  ActionSpecConstraints ('ActionSpec _ _ constraints _) = constraints

-- | Extracts the parameter list (@[(Symbol, Type)]@) from a 'TypedActionSpec'.
type family ActionSpecParameters (spec :: TypedActionSpec) :: [(Symbol, Type)] where
  ActionSpecParameters ('ActionSpec _ _ _ params) = params

-- ============================================================================
-- 3. COLLECTION OPERATIONS & UTILITIES
-- ============================================================================

-- | Type-level list concatenation. Poly-kinded to work with '[Type].
type family ConcatStateLists (list1 :: [k]) (list2 :: [k]) :: [k] where
  ConcatStateLists '[] list2 = list2
  ConcatStateLists (x ': xs) list2 = x ': ConcatStateLists xs list2

-- | Collects all 'ManagedStates' from a list of 'ValidatorDef's..
type family AllManagedStates (validators :: [ValidatorDef]) :: [Type] where
  AllManagedStates '[] = '[]
  AllManagedStates ('Validator v ': rest) = ConcatStateLists (ManagedStates v) (AllManagedStates rest)

-- | Extracts all 'TypedOperation's embedded within a list of 'ActionStep's.
type family ExtractOpsFromActionSteps (steps :: [ActionStep]) :: [TypedOperation] where
  ExtractOpsFromActionSteps '[] = '[]
  ExtractOpsFromActionSteps ('Op op ': rest) = op ': ExtractOpsFromActionSteps rest
  ExtractOpsFromActionSteps ('Let _ op ': rest) = op ': ExtractOpsFromActionSteps rest
  ExtractOpsFromActionSteps ('Map op _ _ ': rest) = op ': ExtractOpsFromActionSteps rest

-- | Extracts the target state tag for each operation in a list.
type family ExtractStateTypes (ops :: [TypedOperation]) :: [Type] where
  ExtractStateTypes '[] = '[]
  ExtractStateTypes (op ': rest) = ExtractStateType op ': ExtractStateTypes rest

-- | Extracts all 'TypedOperation's contained within a single 'TypedActionSpec'.
type family ExtractOpsFromAction (action :: TypedActionSpec) :: [TypedOperation] where
  ExtractOpsFromAction ('ActionSpec _ steps _ _) = ExtractOpsFromActionSteps steps

-- | Extracts all top-level 'TypedConstraint's from a single 'TypedActionSpec'.
type family ExtractConstraintsFromAction (action :: TypedActionSpec) :: [TypedConstraint] where
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

-- | Constraint requiring that all state types in a list have 'Generic' instances
-- for their underlying data type.
type family AllStateTypesGeneric (states :: [Type]) :: Constraint where
  AllStateTypesGeneric '[] = ()
  AllStateTypesGeneric (s ': rest) = 
    ( Generic (StateDatum s)
    , AllStateTypesGeneric rest
    )

-- | Helper for 'ExtractPlutusVersion': Extracts the 'PlutusVersion' from the first validator in the list.
type family ExtractPlutusVersionFromValidators (validators :: [ValidatorDef]) :: PlutusVersion where
  ExtractPlutusVersionFromValidators ('Validator v ': _) = ValidatorPlutusVersion v
  ExtractPlutusVersionFromValidators '[] = 
    TypeError ('Text "Cannot extract Plutus version from empty validator list")

-- | A conditional constraint helper that applies 'StateDatumConstraints'
-- | only if the 'Maybe Type' is a 'Just s'.
type family MaybeStateDatumConstraints (mt :: Maybe Type) :: Constraint where
  MaybeStateDatumConstraints 'Nothing = ()
  MaybeStateDatumConstraints ('Just s) = StateDatumConstraints s
