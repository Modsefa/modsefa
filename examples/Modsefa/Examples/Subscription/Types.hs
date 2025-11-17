{-|
Module      : Modsefa.Examples.Subscription.Types
Description : Defines data types and state types for the Subscription example application.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module defines:
1.  The type-level state tags (e.g., 'ServiceConfigState', 'CustomerSubscriptionState')
    used by the Subscription application.
2.  'StateSpec' instances for each state tag, defining their on-chain properties
    (datum fields, identifier, and reference strategy). The actual datum
    types are generated from these specifications in
    'Modsefa.Examples.Subscription.Generated.Datums'.
-}
{-# LANGUAGE TypeFamilies #-}

module Modsefa.Examples.Subscription.Types
  ( -- * State Type Aliases
    CouponState
  , CustomerSubscriptionState
  , PricingTierState
  , ServiceConfigState
  , TreasuryAdaState
  ) where


import PlutusLedgerApi.V1 (AssetClass)
import PlutusLedgerApi.V3
  ( BuiltinByteString, POSIXTime, PubKeyHash, ScriptHash, TxOutRef
  )

import Modsefa.Core.Foundation
  ( AdaPolicy, AdaTokenName, RefStrategy(..), SpecPolicySource(OwnPolicySpec)
  , SpecStateIdentifier(AggregateAssetSpec, TokenIdentifiedSpec)
  , StateSpec(..)
  )


-- ============================================================================
-- 1. Data Types
-- ============================================================================

-- ============================================================================
-- 2. State Tags
-- ============================================================================

-- | Type-level tag for the singleton service configuration state.
data ServiceConfigState
-- | Type-level tag for the pricing tier states.
data PricingTierState
-- | Type-level tag for the coupon states (mappable for batch operations).
data CouponState
-- | Type-level tag for the customer subscription states.
data CustomerSubscriptionState
-- | Type-level tag for the aggregate ADA treasury state.
data TreasuryAdaState

-- ============================================================================
-- 3. StateSpec Instances
-- ============================================================================

-- | Specification for the 'ServiceConfigState'.
instance StateSpec ServiceConfigState where
  type DatumName ServiceConfigState = "ServiceConfig"
  type DatumFields ServiceConfigState = 
    '[ '("serviceConfigName", BuiltinByteString)
     , '("serviceConfigProvider", PubKeyHash) 
     ]
  -- | Identified by a unique "ServiceConfig" token minted by its own validator.
  type Identifier ServiceConfigState = 'TokenIdentifiedSpec 'OwnPolicySpec "ServiceConfig" 1
  -- | This is a singleton state; it can only be referenced as 'TypedTheOnlyInstance'.
  type Strategy ServiceConfigState = 'OnlyAsUnique

-- | Specification for the 'PricingTierState'.
instance StateSpec PricingTierState where
  type DatumName PricingTierState = "PricingTier"
  type DatumFields PricingTierState = 
    '[ '("pricingTierName", BuiltinByteString)
     , '("pricingTierPrice", Integer)
     , '("pricingTierAssetClass",AssetClass)
     , '("pricingTierBillingPeriod",POSIXTime)
     , '("pricingTierContractLength",POSIXTime)
     ]
  -- | Identified by a unique "PricingTier" token minted by its own validator.
  type Identifier PricingTierState = 'TokenIdentifiedSpec 'OwnPolicySpec "PricingTier" 1
  -- | Multiple tiers can exist; they must be referenced by their properties
  -- | (e.g., finding the unique one where "pricingTierName" matches).
  type Strategy PricingTierState = 'OnlyByProperty

-- | Specification for the 'CouponState'.
instance StateSpec CouponState where
  type DatumName CouponState = "Coupon"
  type DatumFields CouponState = 
    '[ '("couponId", Integer)
     , '("couponBatchId", TxOutRef)
     , '("couponDiscountPercent",Integer)
     ]
  -- | Identified by a unique "Coupon" token minted by its own validator.
  type Identifier CouponState = 'TokenIdentifiedSpec 'OwnPolicySpec "Coupon" 1
  -- | Multiple coupons can exist; they are referenced by their properties.
  type Strategy CouponState = 'OnlyByProperty
  -- | Enable 'Mappable' for batch 'Create'/'Delete' operations.
  type HasMappable CouponState = 'True

-- | Specification for the 'CustomerSubscriptionState'.
instance StateSpec CustomerSubscriptionState where
  type DatumName CustomerSubscriptionState = "CustomerSubscription"
  type DatumFields CustomerSubscriptionState = 
    '[ '("customerSubscriptionPkh", PubKeyHash)
     , '("customerSubscriptionPrice", Integer)
     , '("customerSubscriptionAssetClass",AssetClass)
     , '("customerSubscriptionBillingPeriod",POSIXTime)
     , '("customerSubscriptionContractEndDate",POSIXTime)
     , '("customerSubscriptionPaidThrough",POSIXTime)
     , '("customerSubscriptionServiceProviderValidator",ScriptHash)
     ]
  -- | Identified by a unique "CustomerSubscription" token minted by its own validator.
  type Identifier CustomerSubscriptionState = 'TokenIdentifiedSpec 'OwnPolicySpec "CustomerSubscription" 1
  -- | Multiple subscriptions can exist; referenced by property.
  type Strategy CustomerSubscriptionState = 'OnlyByProperty

-- | Specification for the 'TreasuryAdaState'.
instance StateSpec TreasuryAdaState where
  type DatumName TreasuryAdaState = "TreasuryAda"
  -- | Aggregate states have no datum fields.
  type DatumFields TreasuryAdaState = '[]
  -- | Identified by the total ADA (AdaPolicy, AdaTokenName) at the validator address.
  type Identifier TreasuryAdaState = 'AggregateAssetSpec AdaPolicy AdaTokenName
  -- | Cannot be referenced directly (e.g., 'Update'); only via constraints.
  type Strategy TreasuryAdaState = 'NoRef
  