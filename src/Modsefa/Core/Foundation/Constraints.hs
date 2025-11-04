{-|
Module      : Modsefa.Core.Foundation.Constraints
Description : Constraint aliases and bundles for Modsefa.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC (requires ConstraintKinds, TypeFamilies, etc.)

This module provides convenient constraint aliases (type synonyms for constraints)
that bundle together commonly used constraints throughout the Modsefa library.
Using these aliases reduces boilerplate and improves the readability and
maintainability of function signatures, especially those involving type-level
specifications.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Constraint aliases and bundles for the Modsefa library
--
-- This module provides convenient constraint aliases that bundle together
-- commonly used constraints. This reduces boilerplate and makes function
-- signatures more readable and maintainable.
--
-- Organization:
--   1. Basic Constraint Bundles
--   2. Action & Validation Constraints
--   3. Capability Constraints
--   4. Generic & Serialization Constraints
--   5. Validation Type Families
--   6. Helper Constraint Families
module Modsefa.Core.Foundation.Constraints
  ( -- * Action & Validation Constraints
    ActionConstraints

    -- * Capability Constraints
  , HasValidatorScript
  
    -- * Validation Type Families
  , ValidatorExistsInApp
  , ValidatorParamFieldExists
  , ParamInList
  ) where

import Data.Kind (Constraint, Type)
import Data.Type.Bool (If)
import Data.Type.Equality (type (==))
import Data.Typeable (Typeable)
import GHC.TypeLits (ErrorMessage(..), KnownSymbol, Symbol, TypeError)
import GHC.Generics (Generic)

import PlutusTx (FromData, ToData)

import Modsefa.Core.Foundation.TypeFamilies
  ( ActionSpecConstraints, ActionSpecName, ActionSpecParameters, ActionSpecSteps
  )
import Modsefa.Core.Foundation.Types
  ( ActionStep(Let, Map, Op), AppSpec(Validators), GetStateData, GetStateName
  , StateRepresentable, StateType, TypedActionSpec, TypedConstraint, TypedOperation
  , ValidatorDef(..), ValidatorSpec(Params, ValidatorAppName)
  )


-- ============================================================================
-- 1. BASIC CONSTRAINT BUNDLES
-- ============================================================================

-- | Standard constraints required for working with a validator type @v@.
-- Bundles 'ValidatorSpec', 'KnownSymbol' for its name, and 'Typeable'.
type BasicValidatorConstraints v = 
  ( ValidatorSpec v
  , KnownSymbol (ValidatorAppName v)
  , Typeable v
  )

-- | Common constraints for a 'StateType' @st@ involved in on-chain operations
-- (like create, update, delete). Includes 'StateRepresentableConstraints',
-- 'SerializationConstraints', and 'GenericStateConstraints'.
type StateOperationConstraints st =
  ( StateRepresentableConstraints st
  , SerializationConstraints (GetStateData st)
  , GenericStateConstraints (GetStateData st)
  )

-- | Core constraints for a 'StateType' @st@ to be representable on-chain,
-- excluding serialization but including 'StateRepresentable', 'KnownSymbol' for its name,
-- and 'Typeable' instances for both the state type and its underlying data type.
type StateRepresentableConstraints st =
  ( StateRepresentable st
  , KnownSymbol (GetStateName st)
  , Typeable st
  , Typeable (GetStateData st)
  )

-- ============================================================================
-- 2. ACTION & VALIDATION CONSTRAINTS
-- ============================================================================

-- | Constraints required for a complete 'TypedActionSpec' @action@ within an application @app@.
-- Includes 'KnownSymbol' for the action name, 'ValidActionSpec', and 'ActionParameterConstraints'.
type ActionConstraints app action =
  ( KnownSymbol (ActionSpecName action)
  , ValidActionSpec action
  , ActionParameterConstraints (ActionSpecParameters action)
  )

-- | Constraint indicating a 'TypedActionSpec' @action@ is internally well-formed,
-- meaning its steps and constraints pass basic validation checks.
type ValidActionSpec action = 
  ( ValidateActionSteps (ActionSpecSteps action)
  , ValidateConstraints (ActionSpecConstraints action)
  )

-- | Constraints required for the parameters @params@ of an action.
-- Ensures all parameter names are 'KnownSymbol's and types are 'Typeable'.
type ActionParameterConstraints params = AllParametersValid params

-- ============================================================================
-- 3. CAPABILITY CONSTRAINTS
-- ============================================================================

-- | Constraint indicating a validator @v@ can provide script implementations.
-- Currently equivalent to 'BasicValidatorConstraints'. May be extended later.
type HasValidatorScript v = 
  BasicValidatorConstraints v

-- ============================================================================
-- 4. GENERIC & SERIALIZATION CONSTRAINTS
-- ============================================================================

-- | Standard PlutusTx serialization constraints required for a type @t@ used in datums or redeemers.
-- Includes 'ToData', 'FromData', 'Show', and 'Eq'.
type SerializationConstraints t =
  ( ToData t
  , FromData t
  , Show t
  , Eq t
  )

-- | Constraint requiring a type @t@ to have a 'Generic' instance, needed for
-- deriving functionality like field extraction or generic updates.
type GenericStateConstraints t =
  Generic t

-- ============================================================================
-- 5. VALIDATION TYPE FAMILIES (as Constraints)
-- ============================================================================

-- | Type-level check ensuring a validator named @vName@ exists within the 'Validators' list of application @app@.
-- Produces a 'TypeError' if the validator is not found.
type family ValidatorExistsInApp (vName :: Symbol) (app :: Type) :: Constraint where
  ValidatorExistsInApp vName app = ValidatorInValidatorList vName (Validators app)

-- | Helper for 'ValidatorExistsInApp': Recursively searches the validator list.
type family ValidatorInValidatorList (vName :: Symbol) (validators :: [ValidatorDef]) :: Constraint where
  ValidatorInValidatorList vName ('Validator v ': rest) =
    If (vName == ValidatorAppName v)
      (() :: Constraint)
      (ValidatorInValidatorList vName rest)
  ValidatorInValidatorList vName '[] =
    TypeError ('Text "Validator " ':<>: 'ShowType vName ':<>: 'Text " not found in app")

-- | Type-level check ensuring a validator named @vName@ in application @app@ has a parameter named @param@.
-- Produces a 'TypeError' if the validator or parameter is not found.
type family ValidatorParamFieldExists (vName :: Symbol) (param :: Symbol) (app :: Type) :: Constraint where
  ValidatorParamFieldExists vName param app = 
    ValidatorParamFieldInList vName param (Validators app)

-- | Helper for 'ValidatorParamFieldExists': Finds the validator and checks its parameters.
type family ValidatorParamFieldInList (vName :: Symbol) (param :: Symbol) (validators :: [ValidatorDef]) :: Constraint where
  ValidatorParamFieldInList vName param ('Validator v ': rest) =
    If (vName == ValidatorAppName v)
      (ParamInList param (Params v))
      (ValidatorParamFieldInList vName param rest)
  ValidatorParamFieldInList vName param '[] =
    TypeError ('Text "Validator " ':<>: 'ShowType vName ':<>: 'Text " not found")

-- | Helper for 'ValidatorParamFieldInList': Checks if a parameter name exists in a @[(Symbol, Type)]@ list.
type family ParamInList (param :: Symbol) (params :: [(Symbol, Type)]) :: Constraint where
  ParamInList param ('(param, _) ': _) = ()  -- Found matching parameter
  ParamInList param ('(_, _) ': rest) = ParamInList param rest  -- Keep searching
  ParamInList param '[] = 
    TypeError ('Text "Parameter " ':<>: 'ShowType param ':<>: 'Text " not found in validator parameters")

-- ============================================================================
-- 6. HELPER CONSTRAINT FAMILIES
-- ============================================================================

-- | Validate all steps in an action
type family ValidateActionSteps (steps :: [ActionStep]) :: Constraint where
  ValidateActionSteps '[] = ()
  ValidateActionSteps ('Op op ': rest) = (ValidateOperations '[op], ValidateActionSteps rest)
  ValidateActionSteps ('Let _ op ': rest) = (ValidateOperations '[op], ValidateActionSteps rest)
  ValidateActionSteps ('Map op _ _ ': rest) = (ValidateOperations '[op], ValidateActionSteps rest)

-- | Ensures all validators in a list satisfy 'BasicValidatorConstraints'.
type family AllValidatorsValid (validators :: [ValidatorDef]) :: Constraint where
  AllValidatorsValid '[] = ()
  AllValidatorsValid ('Validator v ': rest) = 
    ( BasicValidatorConstraints v
    , AllValidatorsValid rest
    )

-- | Ensures all states in a list satisfy 'StateOperationConstraints'.
type family AllStatesValid (states :: [StateType]) :: Constraint where
  AllStatesValid '[] = ()
  AllStatesValid (st ': rest) = 
    ( StateOperationConstraints st
    , AllStatesValid rest
    )

-- | Ensures all actions in a list satisfy 'ValidActionSpec'.
type family AllActionsValid (actions :: [TypedActionSpec app]) :: Constraint where
  AllActionsValid '[] = ()
  AllActionsValid (action ': rest) = 
    ( ValidActionSpec action
    , AllActionsValid rest
    )

-- | Ensures all parameters in a @[(Symbol, Type)]@ list satisfy basic constraints ('KnownSymbol', 'Typeable').
type family AllParametersValid (params :: [(Symbol, Type)]) :: Constraint where
  AllParametersValid '[] = ()
  AllParametersValid ('(name, t) ': rest) = 
    ( KnownSymbol name
    , Typeable t
    , AllParametersValid rest
    )

-- | Ensures all validators in a list satisfy 'HasValidatorScript'.
type family AllValidatorsHaveScripts (validators :: [ValidatorDef]) :: Constraint where
  AllValidatorsHaveScripts '[] = ()
  AllValidatorsHaveScripts ('Validator v ': rest) = 
    ( HasValidatorScript v
    , AllValidatorsHaveScripts rest
    )

-- | Ensures all validators in a list satisfy 'BasicValidatorConstraints' (potentially redundant with 'AllValidatorsValid').
type family AllValidatorsWellFormed (validators :: [ValidatorDef]) :: Constraint where
  AllValidatorsWellFormed '[] = ()
  AllValidatorsWellFormed ('Validator v ': rest) = 
    ( BasicValidatorConstraints v
    , AllValidatorsWellFormed rest
    )

-- | Validate operations list (placeholder)
type family ValidateOperations (ops :: [TypedOperation]) :: Constraint where
  ValidateOperations '[] = ()
  ValidateOperations (_ ': rest) = ValidateOperations rest

-- | Validate constraints list (placeholder)  
type family ValidateConstraints (constraints :: [TypedConstraint]) :: Constraint where
  ValidateConstraints '[] = ()
  ValidateConstraints (_ ': rest) = ValidateConstraints rest