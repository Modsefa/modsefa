{-|
Module      : Modsefa.Examples.Subscription.Types
Description : Defines data types and state types for the Subscription example application.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module defines the Haskell records used as on-chain datum types for the
Subscription application, along with their corresponding Modsefa 'StateType' aliases
and 'StateRepresentable' instances. It covers service configuration, pricing tiers,
coupons, customer subscriptions, and the treasury state.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Modsefa.Examples.Subscription.Types
  ( -- * Data Types
    ServiceConfig(..)
  , PricingTier(..)
  , Coupon(..)
  , CustomerSubscription(..)
  , TreasuryAda(..)
    -- * State Type Aliases
  , ServiceConfigState
  , PricingTierState
  , CouponState
  , CustomerSubscriptionState
  , TreasuryAdaState
  ) where

import GHC.Generics (Generic)

import GeniusYield.Types (GYAssetClass(GYLovelace))
import PlutusLedgerApi.V1 (AssetClass)
import PlutusLedgerApi.V3
  ( BuiltinByteString, POSIXTime, PubKeyHash, ScriptHash, TxOutRef
  )
import qualified PlutusTx

import Modsefa.Core.Foundation
  ( PolicySource(OwnPolicy), RefStrategy(..)
  , StateIdentifier(TokenIdentified, AggregateAsset)
  , StateRepresentable(..), StateType(ST)
  )
import Modsefa.Core.Transaction (Mappable)


-- ============================================================================
-- 1. Data Types (Used as On-Chain Datums)
-- ============================================================================

-- | Configuration for the subscription service. Expected to be a singleton state per app instance.
data ServiceConfig = ServiceConfig
  { serviceConfigName :: BuiltinByteString -- ^ Display name of the service.
  , serviceConfigProvider :: PubKeyHash -- ^ PKH authorized to manage the service (update config, create tiers, withdraw).
  } deriving (Eq, Show, Generic)
PlutusTx.makeLift ''ServiceConfig
PlutusTx.makeIsDataIndexed ''ServiceConfig [('ServiceConfig, 0)]

-- | Defines a specific pricing plan for the service. Multiple tiers can exist.
data PricingTier = PricingTier
  { pricingTierName :: BuiltinByteString -- ^ Identifier name for this tier (e.g., "Basic", "Premium").
  , pricingTierPrice :: Integer -- ^ Cost per billing period in units of 'pricingTierAssetClass'.
  , pricingTierAssetClass :: AssetClass -- ^ The asset (e.g., Lovelace, a stablecoin) used for payment.
  , pricingTierBillingPeriod :: POSIXTime -- ^ Duration of one billing cycle (e.g., 30 days in milliseconds).
  , pricingTierContractLength :: POSIXTime -- ^ Minimum subscription duration (e.g., 365 days in milliseconds).
  } deriving (Eq, Show, Generic)
PlutusTx.makeLift ''PricingTier
PlutusTx.makeIsDataIndexed ''PricingTier [('PricingTier, 0)]
-- Instance needed if this type is used in a 'Map' operation (e.g., batch creating tiers).
instance Mappable PricingTier

-- | Represents a discount coupon that can be applied during subscription.
data Coupon = Coupon
  { couponId :: Integer -- ^ Unique identifier for the coupon within its batch.
  , couponBatchId :: TxOutRef -- ^ UTxO reference identifying the batch this coupon belongs to. Used for uniqueness and deletion.
  , couponDiscountPercent :: Integer -- ^ Discount percentage (e.g., 15 for 15%).
  } deriving (Eq, Show, Generic)
PlutusTx.makeLift ''Coupon
PlutusTx.makeIsDataIndexed ''Coupon [('Coupon, 0)]
-- Instance needed for batch creation/deletion via 'Map'.
instance Mappable Coupon

-- | Represents an active subscription for a specific customer.
data CustomerSubscription = CustomerSubscription
  { customerSubscriptionPkh :: PubKeyHash -- ^ PKH of the subscribing customer.
  , customerSubscriptionPrice :: Integer -- ^ Price per period paid (potentially discounted).
  , customerSubscriptionAssetClass :: AssetClass -- ^ Asset used for payment.
  , customerSubscriptionBillingPeriod :: POSIXTime -- ^ Duration of the billing cycle.
  , customerSubscriptionContractEndDate :: POSIXTime -- ^ Timestamp when the contract term ends.
  , customerSubscriptionPaidThrough :: POSIXTime -- ^ Timestamp through which the subscription is currently paid.
  , customerSubscriptionServiceProviderValidator :: ScriptHash -- ^ Script hash of the ServiceAndPricing validator (identifies the service subscribed to).
  } deriving (Eq, Show, Generic)
PlutusTx.makeLift ''CustomerSubscription
PlutusTx.makeIsDataIndexed ''CustomerSubscription [('CustomerSubscription, 0)]

-- | Represents the aggregated ADA held by the Treasury validator.
-- The actual amount is stored in the UTxO's value, not the datum.
-- The datum is just a placeholder unit type.
newtype TreasuryAda = TreasuryAda () -- Simple unit type wrapper
  deriving (Show, Eq, Generic)
PlutusTx.makeIsDataIndexed ''TreasuryAda [('TreasuryAda, 0)]
PlutusTx.makeLift ''TreasuryAda

-- ============================================================================
-- 2. StateType Definitions (Type Aliases)
-- ============================================================================

-- | 'StateType' alias for the service configuration.
type ServiceConfigState = 'ST "ServiceConfig" ServiceConfig
-- | 'StateType' alias for pricing tiers.
type PricingTierState = 'ST "PricingTier" PricingTier
-- | 'StateType' alias for coupons.
type CouponState = 'ST "Coupon" Coupon
-- | 'StateType' alias for customer subscriptions.
type CustomerSubscriptionState = 'ST "CustomerSubscription" CustomerSubscription
-- | 'StateType' alias for the ADA treasury.
type TreasuryAdaState = 'ST "TreasuryAda" TreasuryAda

-- ============================================================================
-- 3. StateRepresentable Instances
-- ============================================================================

-- | Defines on-chain representation for 'ServiceConfigState'.
instance StateRepresentable ServiceConfigState where
  -- | Identified by a unique "ServiceConfig" token minted by the managing validator ('OwnPolicy').
  stateIdentifier _ = TokenIdentified OwnPolicy "ServiceConfig" 1
  -- | Can only be referenced as the single unique instance ('TypedTheOnlyInstance').
  type AllowedRefStrategy ServiceConfigState = 'OnlyAsUnique

-- | Defines on-chain representation for 'PricingTierState'.
instance StateRepresentable PricingTierState where
  -- | Identified by a unique "PricingTier" token minted by the managing validator ('OwnPolicy').
  stateIdentifier _ = TokenIdentified OwnPolicy "PricingTier" 1
  -- | Can only be referenced using predicates based on properties (like tier name).
  type AllowedRefStrategy PricingTierState = 'OnlyByProperty

-- | Defines on-chain representation for 'CouponState'.
instance StateRepresentable CouponState where
  -- | Identified by a unique "Coupon" token minted by the managing validator ('OwnPolicy').
  stateIdentifier _ = TokenIdentified OwnPolicy "Coupon" 1
  -- | Can only be referenced using predicates based on properties (like coupon ID and batch ID).
  type AllowedRefStrategy CouponState = 'OnlyByProperty

-- | Defines on-chain representation for 'CustomerSubscriptionState'.
instance StateRepresentable CustomerSubscriptionState where
  -- | Identified by a unique "CustomerSubscription" token minted by the managing validator ('OwnPolicy').
  stateIdentifier _ = TokenIdentified OwnPolicy "CustomerSubscription" 1
  -- | Can only be referenced using predicates based on properties.
  type AllowedRefStrategy CustomerSubscriptionState = 'OnlyByProperty

-- | Defines on-chain representation for 'TreasuryAdaState'.
instance StateRepresentable TreasuryAdaState where
  -- | Represents an aggregated amount of Lovelace held at the validator address. Not identified by a unique token.
  stateIdentifier _ = AggregateAsset GYLovelace
  -- | Cannot be directly referenced in operations (use 'MustAddToAggregateState' / 'MustWithdrawFromAggregateState').
  type AllowedRefStrategy TreasuryAdaState = 'NoRef