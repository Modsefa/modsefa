{-|
Module      : Modsefa.Core.Foundation.Validator
Description : Core type definitions for Modsefa validators.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module defines the 'ValidatorSpec' type class and its associated
helper types (e.g., 'ValidatorDef', 'InstanceDef').
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Modsefa.Core.Foundation.Validator
  ( ValidatorDef(..)
  , ValidatorSpec(..)
  , InstanceType(..)
  , AllStatesHaveSpec
  ) where
    
import Data.Kind (Constraint, Type)
import GHC.TypeLits (KnownSymbol, Symbol)

import GeniusYield.Types (PlutusVersion)

import Modsefa.Core.Foundation.Types (StateSpec)

-- ============================================================================
-- * CORE PRIMITIVES
-- ============================================================================

-- | A type-level wrapper that lifts a 'ValidatorSpec' implementor (a 'Type')
-- | into the type-level list format used by 'AppSpec'.
data ValidatorDef where
  Validator :: Type -> ValidatorDef

-- | Specifies the instantiation model for a validator script within an application instance.
data InstanceType
  = SingleInstance   -- ^ The validator script exists as a single instance for the entire application instance.
  | MultiInstance    -- ^ Multiple instances of the validator script can exist, parameterized distinctly (e.g., by customer PKH).
  deriving (Eq, Show)

-- | Defines the specification for an individual validator script within the application.
class
  ( KnownSymbol (ValidatorAppName v)
  , AllStatesHaveSpec (ManagedStates v)
  ) => ValidatorSpec (v :: Type) where
  -- | Parameter types required by this validator's script, specified as a
  -- | type-level list of '(Name, Type)' tuples.
  -- | Example: `type Params MyValidator = '[ '("ownerPkh", PubKeyHash) ]`
  type Params v :: [(Symbol, Type)]
  
  -- | Types of the states whose lifecycle is managed by this validator script.
  -- | Each type in this list must have a 'StateSpec' instance.
  -- | Example: `type ManagedStates MyValidator = '[ ConfigState, UserState ]`
  type ManagedStates v :: [Type]
  
  -- | A unique symbolic name ('Symbol') identifying this valida'.
  type ValidatorAppName v :: Symbol
  
  -- | The Plutus script version ('PlutusVersion') used by this validator.
  -- | Example: `type ValidatorPlutusVersion MyValidator = 'PlutusV3`
  type ValidatorPlutusVersion v :: PlutusVersion
  
  -- | Specifies whether this validator exists as a 'SingleInstance' or 'MultiInstance'
  -- | within the application.
  -- | Example: `type ValidatorInstanceType MyValidator = 'SingleInstance`
  type ValidatorInstanceType v :: InstanceType

-- ============================================================================
-- * CONSTRAINTS
-- ============================================================================

-- | (Internal) A helper constraint that ensures every 'Type' in a list
-- | has a valid 'StateSpec' instance.
-- | This is used as a superclass constraint on 'ValidatorSpec' to
-- | validate the 'ManagedStates' list.
type family AllStatesHaveSpec (states :: [Type]) :: Constraint where
  AllStatesHaveSpec '[] = ()
  AllStatesHaveSpec (s ': rest) = (StateSpec s, AllStatesHaveSpec rest)
