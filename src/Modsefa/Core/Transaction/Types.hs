{-|
Module      : Modsefa.Core.Transaction.Types
Description : Shared data types for Modsefa transaction building.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module defines data types used across multiple modules within the
Modsefa transaction building subsystem (e.g., Builder, Constraints, Operations).
Placing them here helps avoid cyclic dependencies between those modules.
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

-- | Shared types for transaction building
--
-- This module contains data types that are used across multiple
-- modules in the transaction building subsystem to avoid cyclic
-- dependencies.
module Modsefa.Core.Transaction.Types
  ( -- * Redeemer Policy
    RedeemerPolicy(..)
    -- * Derived Parameters
  , SDerivedParams(..)
  , SomeDerivedParams(..)
  ) where

import Data.Kind (Type)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Data.Typeable (Proxy, Typeable)
import PlutusTx.Builtins.Internal (BuiltinByteString)


-- | Specifies how the redeemer for a validator script should be determined
-- during transaction construction for a specific action.
data RedeemerPolicy
  = UseUnitRedeemer -- ^ Use the unit value @()@ as the redeemer.
  | UseNamedRedeemer BuiltinByteString -- ^ Use the action's name (as 'BuiltinByteString') as the redeemer.
  deriving (Show, Eq)

-- | Singleton GADT representing resolved derived parameters ('Modsefa.Core.Foundation.ParamDerivation')
-- at the value level, maintaining type information. The type-level list @params@
-- mirrors the @[(Symbol, Type)]@ structure.
data SDerivedParams (params :: [(Symbol, Type)]) where
  -- | Represents an empty list of derived parameters.
  SDPNil :: SDerivedParams '[]
  -- | Constructs a non-empty list, holding the name ('Proxy'), the resolved value ('t'),
  -- and the rest of the derived parameter list ('SDerivedParams rest').
  SDPCons :: (KnownSymbol name, Typeable t) =>
           Proxy name -> t -> SDerivedParams rest -> SDerivedParams ('(name, t) ': rest)

-- | Show instance for 'SDerivedParams', showing names but hiding potentially large values.
instance Show (SDerivedParams params) where
  show SDPNil = "SDPNil"
  show (SDPCons nameProxy _value rest) =
    "SDPCons(" ++ symbolVal nameProxy ++ ", ...) : " ++ show rest

-- | Existential wrapper for 'SDerivedParams', hiding the specific type-level list @params@.
-- Allows storing or passing resolved derived parameters where the exact set isn't known statically.
data SomeDerivedParams where
  SomeDerivedParams :: SDerivedParams params -> SomeDerivedParams

-- Show instance for the existential wrapper.
instance Show SomeDerivedParams where
    show (SomeDerivedParams sdp) = "SomeDerivedParams (" ++ show sdp ++ ")"