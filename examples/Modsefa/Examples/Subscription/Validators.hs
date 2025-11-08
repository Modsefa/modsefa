{-|
Module      : Modsefa.Examples.Subscription.Validators
Description : Defines ValidatorSpec instances for the Subscription example application's validators.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module defines the type tags and, more importantly, the 'ValidatorSpec'
instances for the different validator scripts used in the Subscription application.
Each instance specifies the parameters, managed states, application name, Plutus version,
and instance type for its corresponding validator.
-}
{-# LANGUAGE TypeFamilies #-}

module Modsefa.Examples.Subscription.Validators
  ( -- * Validator Type Tags
    CouponValidator
  , CustomerValidator
  , ServiceAndPricingValidator
  , TreasuryValidator
  ) where

import GeniusYield.Types (PlutusVersion(PlutusV3))
import PlutusLedgerApi.V3 (Address, PubKeyHash, TxOutRef)

import Modsefa.Core.Foundation
  ( InstanceType(SingleInstance, MultiInstance), ValidatorSpec(..)
  )

import Modsefa.Examples.Subscription.Types
  ( CouponState, TreasuryAdaState, CustomerSubscriptionState
  , PricingTierState, ServiceConfigState
  )


-- ============================================================================
-- Validator Type Tags
-- ============================================================================
-- These are empty data types used solely as type-level identifiers for the different validators.

-- | Type tag for the validator managing 'ServiceConfigState' and 'PricingTierState'.
data ServiceAndPricingValidator
-- | Type tag for the validator managing 'CouponState'.
data CouponValidator
-- | Type tag for the validator managing 'CustomerSubscriptionState'.
data CustomerValidator
-- | Type tag for the validator managing 'TreasuryAdaState'.
data TreasuryValidator

-- ============================================================================
-- ValidatorSpec Instances
-- ============================================================================

-- | Specification for the 'ServiceAndPricingValidator'.
instance ValidatorSpec ServiceAndPricingValidator where
  -- | Requires a 'TxOutRef' named "bootstrapUtxo" for initialization.
  type Params ServiceAndPricingValidator = '[ '("bootstrapUtxo", TxOutRef) ]
  -- | Manages the service configuration and pricing tier states.
  type ManagedStates ServiceAndPricingValidator = '[ServiceConfigState, PricingTierState]
  -- | Identifier name within the 'AppSpec'.
  type ValidatorAppName ServiceAndPricingValidator = "ServiceAndPricingValidator"
  -- | Targets Plutus V3.
  type ValidatorPlutusVersion ServiceAndPricingValidator = 'PlutusV3
  -- | Exists as a single instance per application instance.
  type ValidatorInstanceType ServiceAndPricingValidator = SingleInstance

-- | Specification for the 'CouponValidator'.
instance ValidatorSpec CouponValidator where
  -- | Requires the 'Address' of the corresponding 'ServiceAndPricingValidator' instance,
  -- derived via 'ParameterDerivations'.
  type Params CouponValidator = '[ '("serviceValidatorAddress", Address) ]
  -- | Manages coupon states.
  type ManagedStates CouponValidator = '[CouponState]
  -- | Identifier name within the 'AppSpec'.
  type ValidatorAppName CouponValidator = "CouponValidator"
  -- | Targets Plutus V3.
  type ValidatorPlutusVersion CouponValidator = 'PlutusV3
  -- | Exists as a single instance per application instance (derived from the single Service instance).
  type ValidatorInstanceType CouponValidator = SingleInstance

-- | Specification for the 'CustomerValidator'.
instance ValidatorSpec CustomerValidator where
  -- | Requires the 'PubKeyHash' of the customer ("customerPkh") to parameterize each instance.
  type Params CustomerValidator = '[ '("customerPkh", PubKeyHash) ]
  -- | Manages customer subscription states.
  type ManagedStates CustomerValidator = '[CustomerSubscriptionState]
  -- | Identifier name within the 'AppSpec'.
  type ValidatorAppName CustomerValidator = "CustomerValidator"
  -- | Targets Plutus V3.
  type ValidatorPlutusVersion CustomerValidator = 'PlutusV3
  -- | Multiple instances can exist, one for each customer subscription, parameterized by their PKH.
  type ValidatorInstanceType CustomerValidator = MultiInstance

-- | Specification for the 'TreasuryValidator'.
instance ValidatorSpec TreasuryValidator where
  -- | Requires the 'Address' of the corresponding 'ServiceAndPricingValidator' instance,
  -- derived via 'ParameterDerivations'.
  type Params TreasuryValidator = '[ '("serviceValidatorAddress", Address) ]
  -- | Manages the aggregated ADA treasury state.
  type ManagedStates TreasuryValidator = '[TreasuryAdaState]
  -- | Identifier name within the 'AppSpec'.
  type ValidatorAppName TreasuryValidator = "TreasuryValidator"
  -- | Targets Plutus V3.
  type ValidatorPlutusVersion TreasuryValidator = 'PlutusV3
  -- | Exists as a single instance per application instance (derived from the single Service instance).
  type ValidatorInstanceType TreasuryValidator = SingleInstance