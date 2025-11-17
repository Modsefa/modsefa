{-|
Module      : Modsefa.Core.Foundation.Constraints
Description : Type-level validation constraints for Modsefa specifications.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC (requires ConstraintKinds, TypeFamilies, etc.)

This module provides type-level constraint families used to validate
Modsefa specifications at compile time, primarily within
'Modsefa.Core.Foundation.App'.
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Modsefa.Core.Foundation.Constraints
  ( -- * Validation Type Families
    ValidatorParamFieldInList
  , ValidatorInValidatorList
  ) where

import Data.Kind (Constraint, Type)
import Data.Type.Bool (If)
import Data.Type.Equality (type (==))
import GHC.TypeLits (ErrorMessage(..), Symbol, TypeError)

import Modsefa.Core.Foundation.Validator
  ( ValidatorDef(..), ValidatorSpec(Params, ValidatorAppName)
  )


-- ============================================================================
-- * VALIDATION TYPE FAMILIES (as Constraints)
-- ============================================================================

-- | Helper for 'ValidatorExistsInApp': Recursively searches the validator list.
type family ValidatorInValidatorList (vName :: Symbol) (validators :: [ValidatorDef]) :: Constraint where
  ValidatorInValidatorList vName ('Validator v ': rest) =
    If (vName == ValidatorAppName v)
      (() :: Constraint)
      (ValidatorInValidatorList vName rest)
  ValidatorInValidatorList vName '[] =
    TypeError ('Text "Validator " ':<>: 'ShowType vName ':<>: 'Text " not found in app")

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