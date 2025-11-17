{-|
Module      : Modsefa.Examples.Feed.Validators
Description : Defines the ValidatorSpec instance for the Feed application's validator.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module defines:
1.  A data type 'FeedValidator' used as a type-level tag.
2.  A 'Modsefa.Core.Foundation.Types.ValidatorSpec' instance for 'FeedValidator', specifying its parameters, managed states, name, Plutus version, and instance type.
-}
{-# LANGUAGE TypeFamilies #-}

module Modsefa.Examples.Feed.Validators
  ( -- * Validator Type Tag
    FeedValidator
  ) where

import GeniusYield.Types (PlutusVersion(PlutusV3))
import PlutusLedgerApi.V3 (TxOutRef)

import Modsefa.Core.Foundation (InstanceType (SingleInstance), ValidatorSpec (..))

import Modsefa.Examples.Feed.Types (FeedConfigState, FeedDataState)


-- ============================================================================
-- Validator Type Tags
-- ============================================================================

-- | A type-level tag representing the single validator used in the Feed application.
-- This data type itself has no value-level constructors; it's used solely at the type level.
data FeedValidator

-- ============================================================================
-- ValidatorSpec Instances
-- ============================================================================

-- | Defines the specification for the 'FeedValidator'.
instance ValidatorSpec FeedValidator where
  -- | Specifies the parameters required by the FeedValidator script.
  -- Here, it requires a single parameter named "bootstrapUtxo" of type 'TxOutRef',
  -- used for the initialization action ('InitializeFeedSpec').
  type Params FeedValidator = '[ '("bootstrapUtxo", TxOutRef) ]
  -- | Specifies the 'StateSpec's managed by this validator.
  -- The FeedValidator manages both the configuration ('FeedConfigState') and the data entries ('FeedDataState').
  type ManagedStates FeedValidator = '[FeedConfigState, FeedDataState]
  -- | Provides the symbolic name used to refer to this validator in the 'AppSpec'.
  type ValidatorAppName FeedValidator = "FeedValidator"
  -- | Specifies the target Plutus script version.
  type ValidatorPlutusVersion FeedValidator = 'PlutusV3
  -- | Specifies that this validator represents a single instance within the application context,
  -- parameterized by the "bootstrapUtxo".
  type ValidatorInstanceType FeedValidator = SingleInstance