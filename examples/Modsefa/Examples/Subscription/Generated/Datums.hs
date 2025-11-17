{-|
Module      : Modsefa.Examples.Subscription.Generated.Datums
Description : Generates concrete datum types for the Subscription example.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module's sole purpose is to run the Template Haskell
splices that generate the on-chain datum types (e.g., 'ServiceConfig')
from their 'StateSpec' definitions.

It imports the 'StateSpec' instances from 'Modsefa.Examples.Subscription.Types'
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Modsefa.Examples.Subscription.Generated.Datums
  ( Coupon(..)
  , CustomerSubscription(..)
  , PricingTier(..)
  , ServiceConfig(..)
  , TreasuryAda(..)
  ) where

import PlutusLedgerApi.V1 (AssetClass)
import PlutusLedgerApi.V3
  ( BuiltinByteString, POSIXTime, PubKeyHash, ScriptHash, TxOutRef
  )

import Modsefa.CodeGen.Generation (generateStateDatum, generateStateInstances)

import Modsefa.Examples.Subscription.Types
  ( CouponState, CustomerSubscriptionState, PricingTierState, ServiceConfigState
  , TreasuryAdaState
  )


-- ============================================================================
-- 1. Generate Datum Types
-- ============================================================================

$(generateStateDatum @CouponState)
$(generateStateDatum @CustomerSubscriptionState)
$(generateStateDatum @PricingTierState)
$(generateStateDatum @ServiceConfigState)
$(generateStateDatum @TreasuryAdaState)

-- ============================================================================
-- 2. Generate Instances
-- ============================================================================
-- We run this splice after the data types above have been created, due
-- to staging restrictions.

$(generateStateInstances)