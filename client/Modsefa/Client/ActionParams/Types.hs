{-|
Module      : Modsefa.Client.ActionParams.Types
Description : Defines the ToSParamTuple typeclass.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC (requires TypeFamilies)

This module defines the 'ToSParamTuple' typeclass in a separate file
to avoid cyclic dependencies with Template Haskell.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Modsefa.Client.ActionParams.Types
  ( ToSParamTuple(..)
  ) where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)

import Modsefa.Core.Singletons (SParamTuple)


-- | A type class to convert from a standard Haskell tuple (like (a, b, c))
-- | into the type-safe SParamTuple GADT.
class ToSParamTuple (tuple :: Type) (params :: [(Symbol, Type)]) where
  -- | Converts the Haskell tuple into an 'SParamTuple'.
  toSParamTuple :: tuple -> SParamTuple params