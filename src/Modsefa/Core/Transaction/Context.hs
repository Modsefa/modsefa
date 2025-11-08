{-|
Module      : Modsefa.Core.Transaction.Context
Description : Defines the context and monad for Modsefa transaction building.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module defines the 'TxBuilderContext', which holds the state during
transaction construction (like current time, resolved 'Let' bindings), and
the 'TxBuilder' monad transformer stack ('StateT' over 'IO') used to manage
this context and perform lookups or other 'IO' actions required during the build.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

module Modsefa.Core.Transaction.Context
  ( -- * Existential Wrappers
    SomeGYTxOut(..)
  , OperationResult(..)

    -- * Transaction Building Context
  , TxBuilderContext(..)
  , TxBuilder(..)

    -- * Type Aliases
  , DerivationContext
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState, StateT)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

import GeniusYield.Types (GYTxOut, PlutusVersion, SingPlutusVersionI)
import PlutusLedgerApi.V3 (POSIXTime)

import Modsefa.Core.Foundation.Types (SomeFieldValue, SomeStatedUTxO)


-- | Type alias for the map holding resolved derived parameter values ('ParamDerivation').
-- Keys are the derived parameter names ('Symbol'), values are 'SomeFieldValue'.
type DerivationContext = Map Text SomeFieldValue

-- | Existential wrapper for 'GYTxOut', hiding the Plutus version @pv@ but
-- preserving the 'SingPlutusVersionI' constraint. Useful for storing outputs
-- from 'Create' operations in the context when the exact version might vary or isn't needed.
data SomeGYTxOut (pv :: PlutusVersion) where
  SomeGYTxOut :: (SingPlutusVersionI pv) => GYTxOut pv -> SomeGYTxOut pv

-- | Manual 'Show' instance for 'SomeGYTxOut' to avoid showing potentially large TxOut details.
instance Show (SomeGYTxOut pv) where
  show (SomeGYTxOut _) = "SomeGYTxOut <...>"

-- | Represents the result of processing a 'Modsefa.Core.Foundation.Types.TypedOperation'
-- within a 'Modsefa.Core.Foundation.Types.Let' step. Used to store results in 'tbcLetResults'.
data OperationResult (pv :: PlutusVersion) where
  -- | Result of a 'Modsefa.Core.Foundation.Types.Create' operation, holding the created output.
  ORCreate :: SomeGYTxOut pv -> OperationResult pv
  -- | Result of a 'Modsefa.Core.Foundation.Types.Reference' operation, holding the resolved stated UTxO.
  ORReference :: SomeStatedUTxO -> OperationResult pv

-- | Manual 'Show' instance for 'OperationResult'.
instance Show (OperationResult pv) where
  show (ORCreate so) = "ORCreate (" ++ show so ++ ")"
  show (ORReference _) = "ORReference <...>"

-- | The state carried within the 'TxBuilder' monad during transaction construction.
data TxBuilderContext (pv :: PlutusVersion) = TxBuilderContext
  { tbcCurrentTime :: POSIXTime -- ^ The current on-chain time (lower bound of validity range), used for 'Modsefa.Core.Foundation.Types.CurrentTime'.
  , tbcLetResults :: Map Text (OperationResult pv) -- ^ Results of processed 'Modsefa.Core.Foundation.Types.Let' steps, keyed by label ('Symbol').
  , tbcResolvedRefs :: Map Text SomeStatedUTxO -- ^ Cache of resolved 'Modsefa.Core.Foundation.Types.TypedStateRef's (from 'Reference' ops), keyed by a representation of the 'SStateRef'.
  , tbcResolvedFields :: Map Text SomeFieldValue -- ^ (Currently Unused?) Cache for resolved field values.
  } deriving (Generic, Show)

-- | The monad transformer stack used for building transaction skeletons ('GYTxSkeleton').
-- It combines 'StateT' (carrying the 'TxBuilderContext') over 'IO' (allowing blockchain queries).
newtype TxBuilder pv a = TxBuilder
  { -- | Runs the 'TxBuilder' computation given an initial context, returning the result in 'IO'.
    runTxBuilder :: StateT (TxBuilderContext pv) IO a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO -- ^ Allows lifting 'IO' actions (e.g., 'runGYTxQueryMonadIO').
             , MonadState (TxBuilderContext pv) -- ^ Provides access to the 'TxBuilderContext'.
             )