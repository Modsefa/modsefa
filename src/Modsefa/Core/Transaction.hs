{-|
Module      : Modsefa.Core.Transaction
Description : Re-export module for the Modsefa transaction building subsystem.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module serves as the primary entry point for the Modsefa transaction
building subsystem. It re-exports all core functionality related to constructing
transaction skeletons ('GeniusYield.TxBuilder.GYTxSkeleton') from Modsefa action
specifications. This includes modules for:

- High-level building ('Modsefa.Core.Transaction.Builder')
- Constraint processing ('Modsefa.Core.Transaction.Constraints')
- Operation processing ('Modsefa.Core.Transaction.Operations')
- Parameter handling ('Modsefa.Core.Transaction.Parameters')
- Predicate evaluation ('Modsefa.Core.Transaction.Predicate')
- Shared types ('Modsefa.Core.Transaction.Types')
- Analysis helpers ('Modsefa.Core.Transaction.Analysis')
- Utility functions ('Modsefa.Core.Transaction.Utils')
- The transaction building monad and context ('Modsefa.Core.Transaction.Context')

Import this module for convenient access to all transaction-building elements:

@
import Modsefa.Core.Transaction
@
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Transaction building for the Modsefa library
--
-- This module provides the complete transaction building API for the Modsefa
-- library. It re-exports functionality from the specialized Transaction modules
-- to provide a convenient single import point for transaction operations.
--
-- The Transaction layer is designed to:
--   - Provide a stable API for transaction building operations
--   - Consolidate commonly used transaction imports
--   - Establish consistent naming conventions for transaction functions
--   - Bundle constraint aliases for cleaner transaction code
--
-- Usage:
--   Import this module to get access to all transaction functionality:
--
--   @
--   import Modsefa.Core.Transaction
--   @
--
--   This gives you access to all functions needed for most transaction
--   building operations.
module Modsefa.Core.Transaction
  ( -- * Re-exports from Transaction modules
    module Modsefa.Core.Transaction.Analysis
  , module Modsefa.Core.Transaction.Builder
  , module Modsefa.Core.Transaction.Constraints
  , module Modsefa.Core.Transaction.Context
  , module Modsefa.Core.Transaction.Operations
  , module Modsefa.Core.Transaction.Parameters
  , module Modsefa.Core.Transaction.Predicate
  , module Modsefa.Core.Transaction.Types
  , module Modsefa.Core.Transaction.Utils

    -- * Constraint convenience aliases
  , OperationConstraints
  ) where

import Data.Typeable (Typeable)

import GeniusYield.Types (SingPlutusVersionI)

import Modsefa.Core.Foundation
  ( AllStateTypesGeneric, AppSpec(AppInstanceParameters, Validators)
  , ExtractStateTypes, ParamsToValue, ResolveInstanceParamList
  )
import Modsefa.Core.ValidatorScript (AppValidatorScripts)

-- Transaction modules re-exported
import Modsefa.Core.Transaction.Analysis
import Modsefa.Core.Transaction.Builder
import Modsefa.Core.Transaction.Constraints
import Modsefa.Core.Transaction.Context
import Modsefa.Core.Transaction.Operations
import Modsefa.Core.Transaction.Parameters
import Modsefa.Core.Transaction.Predicate
import Modsefa.Core.Transaction.Types
import Modsefa.Core.Transaction.Utils


-- ============================================================================
-- CONVENIENCE CONSTRAINT ALIASES
-- ============================================================================

-- | Bundles common constraints required when processing a list of 'Modsefa.Core.Foundation.Types.TypedOperation's.
-- Includes checks for 'Generic' instances ('AllStateTypesGeneric'), script availability ('AppValidatorScripts'),
-- Plutus version consistency ('SingPlutusVersionI'), and representable instance parameters ('Typeable').
type OperationConstraints app ops pv =
  ( AllStateTypesGeneric (ExtractStateTypes ops)
  , AppValidatorScripts app
  , SingPlutusVersionI pv
  , Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
  )