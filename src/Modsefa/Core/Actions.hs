{-|
Module      : Modsefa.Core.Actions
Description : Defines the TypedAction proof token for validated action specifications.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module defines the 'TypedAction' data type, which acts as a "proof token".
A value of this type can only be created via the 'mkTypedAction' smart constructor,
which requires the underlying 'TypedActionSpec' to satisfy the 'IsActionSpecValid'
constraint. This enforces compile-time validation of action specifications.
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Modsefa.Core.Actions 
  ( TypedAction
  , mkTypedAction
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy)

import Modsefa.Core.Foundation (IsActionSpecValid, TypedActionSpec)


-- | A "proof token" GADT representing a validated action specification.
-- A value of type @'TypedAction' spec@ serves as proof that the type-level
-- specification @spec :: 'TypedActionSpec' app@ has passed all compile-time
-- validation checks defined by the 'IsActionSpecValid' constraint.
-- The constructor 'UnsafeMkTypedAction' is not exported, ensuring construction
-- only happens via the 'mkTypedAction' smart constructor.
data TypedAction (app :: Type) (spec :: TypedActionSpec) where
  -- Internal constructor, not exported.
  UnsafeMkTypedAction :: TypedAction app spec

-- | Smart constructor for 'TypedAction'.
-- Takes a 'Proxy' for the action specification @spec@ and requires the
-- 'IsActionSpecValid' constraint to hold for that @spec@.
-- Attempting to call this function for an invalid specification will result
-- in a compile-time type error, enforcing the validation rules.
mkTypedAction :: forall app spec.
  (IsActionSpecValid app spec) 
  => Proxy app    -- ^ Proxy for the app to validate against
  -> Proxy spec -- ^ A proxy indicating the specific 'TypedActionSpec' to validate and represent.
  -> TypedAction app spec -- ^ The resulting proof token if validation succeeds.
mkTypedAction _ _ = UnsafeMkTypedAction