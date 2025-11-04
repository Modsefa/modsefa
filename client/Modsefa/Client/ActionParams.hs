{-|
Module      : Modsefa.Client.ActionParams
Description : Convenience syntax and typeclasses for constructing Modsefa action parameters.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module provides two ways to construct the 'SParamTuple' GADT required
by the transaction building system:

1.  **Manual Construction:** A simple infix operator '(+:)' and a pattern 'End'
    to build the tuple step-by-step (e.g., `param1 +: param2 +: End`).
2.  **Automatic Conversion:** The 'ToSParamTuple' type class, which converts
    a standard Haskell tuple (e.g., `(param1, param2)`) directly into the
    corresponding 'SParamTuple'. This is used by the generic 'runAction' function.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Modsefa.Client.ActionParams
  ( -- * Manual Construction
    (+:)
  , pattern End
    -- * Automatic Conversion
  , ToSParamTuple(..)
  ) where

import Data.Typeable (Typeable)

import PlutusLedgerApi.V3 (ToData)

import Modsefa.Core.Singletons (SParamTuple(..))

import Modsefa.Client.ActionParams.TH (makeToSParamTupleInstances)
import Modsefa.Client.ActionParams.Types (ToSParamTuple(..))


-- ============================================================================
-- * Manual Parameter Construction
-- ============================================================================

-- | Infix operator (alias for 'STupleCons') to prepend a value to an 'SParamTuple'.
-- Builds the parameter tuple step-by-step from left to right.
-- The type @t@ must satisfy constraints required by 'STupleCons' ('Typeable', 'ToData', 'Eq').
(+:) :: (Typeable t, ToData t, Eq t)
     => t -- ^ The value to prepend.
     -> SParamTuple rest -- ^ The rest of the parameter tuple.
     -> SParamTuple ('(name, t) ': rest) -- ^ The resulting tuple with the value prepended. Note: 'name' is phantom here.
(+:) = STupleCons

-- | Right-associative infix operator declaration for '(+:)'.
infixr 5 +:

-- | Pattern synonym (alias for 'STupleNil') representing the end of an 'SParamTuple' list.
-- Used to terminate the chain of '(+:)' operators.
pattern End :: SParamTuple '[]
pattern End = STupleNil

-- ============================================================================
-- * Automatic Tuple Conversion
-- ============================================================================

-- Case 0: Empty tuple ()
instance ToSParamTuple () '[] where
  toSParamTuple () = STupleNil

-- Case 1: Single value 't' (for actions with 1 param)
instance (Typeable t, ToData t, Eq t) => ToSParamTuple t '[ '(name, t) ] where
  toSParamTuple val = STupleCons val STupleNil

-- Generate instances for 2-tuples up to 10-tuples
$(makeToSParamTupleInstances 10)