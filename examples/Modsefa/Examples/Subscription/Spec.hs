{-|
Module      : Modsefa.Examples.Subscription.Spec
Description : Defines application and action specifications for the Subscription example.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module defines the core Modsefa specification for a subscription service application.
It includes:
1.  The 'SubscriptionApp' type tag.
2.  The 'AppSpec' instance for 'SubscriptionApp', detailing its validators, states, transitions, instance parameters, and parameter derivations.
3.  Type aliases for each 'TypedActionSpec' (e.g., 'InitializeServiceSpec', 'CreatePricingTierSpec', 'SubscribeSpec', etc.).
4.  Validated 'TypedAction' tokens for each action specification, ensuring compile-time validity.
-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Modsefa.Examples.Subscription.Spec
  ( -- * Application Type Tag
    SubscriptionApp
    -- * Action Specification Types
  , InitializeServiceSpec
  , CreatePricingTierSpec
  , UpdateServiceConfigSpec
  , UpdateServiceProviderSpec
  , BatchCreateCouponsSpec
  , BatchDeleteCouponsSpec
  , SubscribeSpec
  , WithdrawTreasurySpec
  , SubscribeWithCouponSpec
    -- * Validated Action Tokens
  , initializeSubscriptionSpec
  , createPricingTierSpec
  , updateServiceConfigSpec
  , updateServiceProviderSpec
  , batchCreateCouponSpec
  , batchDeleteCouponsSpec
  , subscribeSpec
  , withdrawTreasurySpec
  , subscribeWithCouponSpec
  ) where

import Data.Proxy (Proxy(Proxy))

import PlutusLedgerApi.V1 (AssetClass)
import PlutusLedgerApi.V3 
  ( Address, BuiltinByteString, POSIXTime, PubKeyHash, TxOutRef
  )

import Modsefa.Core.Actions (TypedAction, mkTypedAction)
import Modsefa.Core.Foundation
  ( ActionStep(Let, Map, Op), AppSpec(..), CollectionConstraint(MustHaveUniqueField)
  , DerivationSource(ValidatorAddress, ValidatorHash), FieldSpec(Preserve, SetTo)
  , ParamDerivation(DeriveParam), TypedActionSpec(ActionSpec), TypedConstraint(..)
  , TypedOperation(Create, Delete, Reference, Update)
  , TypedPredicate(And, FieldEquals)
  , TypedStateRef(TypedByLabel, TypedTheOnlyInstance, TypedUniqueWhere)
  , TypedValue(..), ValidatorDef(Validator)
  )

import Modsefa.Examples.Subscription.Generated.Datums (Coupon)
import Modsefa.Examples.Subscription.Types
  ( CouponState, CustomerSubscriptionState, PricingTierState, ServiceConfigState
  , TreasuryAdaState
  )
import Modsefa.Examples.Subscription.Validators
  ( CouponValidator, CustomerValidator, ServiceAndPricingValidator
  , TreasuryValidator
  )


-- ============================================================================
-- Application Specification
-- ============================================================================

-- | A type-level tag representing the Subscription application.
data SubscriptionApp

-- | Defines the overall structure and rules for the 'SubscriptionApp'.
instance AppSpec SubscriptionApp where
  -- | Lists the validators involved in this application.
  type Validators SubscriptionApp =
    '[ 'Validator ServiceAndPricingValidator
     , 'Validator CouponValidator
     , 'Validator CustomerValidator
     , 'Validator TreasuryValidator
     ]
  -- | Defines the high-level application states for state machine tracking.
  type AppStates SubscriptionApp = '["Uninitialized", "ServiceActive"]
  -- | Specifies the initial state.
  type InitialAppState SubscriptionApp = "Uninitialized"
  -- | Defines valid state transitions triggered by specific actions.
  type ActionTransitions SubscriptionApp =
    '[ '(InitializeServiceSpec, "Uninitialized", "ServiceActive")
     , '(CreatePricingTierSpec, "ServiceActive", "ServiceActive")
     , '(UpdateServiceConfigSpec, "ServiceActive", "ServiceActive")
     , '(UpdateServiceProviderSpec, "ServiceActive", "ServiceActive")
     , '(BatchCreateCouponsSpec, "ServiceActive", "ServiceActive")
     , '(BatchDeleteCouponsSpec, "ServiceActive", "ServiceActive")
     , '(SubscribeSpec, "ServiceActive", "ServiceActive")
     , '(WithdrawTreasurySpec, "ServiceActive", "ServiceActive")
     , '(SubscribeWithCouponSpec, "ServiceActive", "ServiceActive")
     ]
  -- | Specifies instance parameters. Requires the "bootstrapUtxo" for the main validator.
  type AppInstanceParameters SubscriptionApp =
    '[ '("ServiceAndPricingValidator", "bootstrapUtxo") ]
  -- | Defines rules for deriving parameters used in actions or other validators.
  type ParameterDerivations SubscriptionApp =
    '[ -- Derive the address of the main validator and name it "serviceValidatorAddress"
       'DeriveParam "serviceValidatorAddress"
          ('ValidatorAddress "ServiceAndPricingValidator")
       -- Derive the script hash of the main validator and name it "derivedServiceProviderValidatorHash"
     , 'DeriveParam "derivedServiceProviderValidatorHash"
          ('ValidatorHash "ServiceAndPricingValidator")
     ]

-- ============================================================================
-- Action Specifications
-- ============================================================================

-- | Specification for the "InitializeService" action.
-- Creates the 'ServiceConfigState' and a default 'PricingTierState'.
-- Requires spending the bootstrap UTxO.
type InitializeServiceSpec =
  'ActionSpec "InitializeService"
    '[ -- List of ActionSteps:
       'Op ('Create @ServiceConfigState -- Create ServiceConfig
         '[ 'SetTo "serviceConfigName" ('ParamValue "serviceName")
          , 'SetTo "serviceConfigProvider" ('ParamValue "serviceProvider")
          ]
         '[])
     , 'Op ('Create @PricingTierState -- Create initial PricingTier
         '[ 'SetTo "pricingTierName" ('ParamValue "tierName")
          , 'SetTo "pricingTierPrice" ('ParamValue "price")
          , 'SetTo "pricingTierAssetClass" ('ParamValue "assetClass")
          , 'SetTo "pricingTierBillingPeriod" ('ParamValue "billingPeriod")
          , 'SetTo "pricingTierContractLength" ('ParamValue "contractLength")
          ]
         '[])
     ]
    '[ -- Constraints:
       'MustSpendValidatorParam "ServiceAndPricingValidator" "bootstrapUtxo",
       'MustNotExist @ServiceConfigState 'TypedTheOnlyInstance
     ]
    '[ -- List of action parameters:
       '("serviceName", BuiltinByteString)
     , '("serviceProvider", PubKeyHash)
     , '("tierName", BuiltinByteString)
     , '("price", Integer)
     , '("assetClass", AssetClass)
     , '("billingPeriod", POSIXTime)
     , '("contractLength", POSIXTime)
     ]

-- | Validated token for 'InitializeServiceSpec'.
initializeSubscriptionSpec :: TypedAction SubscriptionApp InitializeServiceSpec
initializeSubscriptionSpec = mkTypedAction (Proxy @SubscriptionApp) (Proxy @InitializeServiceSpec)


-- | Specification for the "CreatePricingTier" action.
-- Creates a new 'PricingTierState'. Requires signature from the service provider.
type CreatePricingTierSpec =
  'ActionSpec "CreatePricingTier"
    '[ -- List of ActionSteps:
       'Op ('Create @PricingTierState -- Create a new PricingTier
         '[ 'SetTo "pricingTierName" ('ParamValue "tierName")
          , 'SetTo "pricingTierPrice" ('ParamValue "price")
          , 'SetTo "pricingTierAssetClass" ('ParamValue "assetClass")
          , 'SetTo "pricingTierBillingPeriod" ('ParamValue "billingPeriod")
          , 'SetTo "pricingTierContractLength" ('ParamValue "contractLength")
          ]
         '[])
     ]
    '[ -- Constraints:
       'MustBeSignedByState @ServiceConfigState 'TypedTheOnlyInstance "serviceConfigProvider"
     ]
    '[ -- List of action parameters:
       '("tierName", BuiltinByteString)
     , '("price", Integer)
     , '("assetClass", AssetClass)
     , '("billingPeriod", POSIXTime)
     , '("contractLength", POSIXTime)
     ]

-- | Validated token for 'CreatePricingTierSpec'.
createPricingTierSpec :: TypedAction SubscriptionApp CreatePricingTierSpec
createPricingTierSpec = mkTypedAction (Proxy @SubscriptionApp) (Proxy @CreatePricingTierSpec)


-- | Specification for the "UpdateServiceConfig" action.
-- Updates the name in the 'ServiceConfigState'. Preserves the provider. Requires provider signature.
type UpdateServiceConfigSpec =
  'ActionSpec "UpdateServiceConfig"
    '[ -- List of ActionSteps:
       'Op ('Update @ServiceConfigState 'TypedTheOnlyInstance -- Update the unique ServiceConfig
         '[ 'SetTo "serviceConfigName" ('ParamValue "newServiceName")
          , 'Preserve "serviceConfigProvider"
          ]
         '[])
     ]
    '[ -- Constraints:
       'MustBeSignedByState @ServiceConfigState 'TypedTheOnlyInstance "serviceConfigProvider"
     ]
    '[ -- List of action parameters:
       '("newServiceName", BuiltinByteString)
     ]

-- | Validated token for 'UpdateServiceConfigSpec'.
updateServiceConfigSpec :: TypedAction SubscriptionApp UpdateServiceConfigSpec
updateServiceConfigSpec = mkTypedAction (Proxy @SubscriptionApp) (Proxy @UpdateServiceConfigSpec)


-- | Specification for the "UpdateServiceProvider" action.
-- Updates the provider PKH in the 'ServiceConfigState'. Preserves the name. Requires old provider signature.
type UpdateServiceProviderSpec =
  'ActionSpec "UpdateServiceProvider"
    '[ -- List of ActionSteps:
       'Op ('Update @ServiceConfigState 'TypedTheOnlyInstance -- Update the unique ServiceConfig
         '[ 'Preserve "serviceConfigName"
          , 'SetTo "serviceConfigProvider" ('ParamValue "newServiceProvider")
          ]
         '[])
     ]
    '[ -- Constraints:
       'MustBeSignedByState @ServiceConfigState 'TypedTheOnlyInstance "serviceConfigProvider"
     ]
    '[ -- List of action parameters:
       '("newServiceProvider", PubKeyHash)
     ]

-- | Validated token for 'UpdateServiceProviderSpec'.
updateServiceProviderSpec :: TypedAction SubscriptionApp UpdateServiceProviderSpec
updateServiceProviderSpec = mkTypedAction (Proxy @SubscriptionApp) (Proxy @UpdateServiceProviderSpec)


-- | Specification for the "BatchCreateCoupons" action.
-- Uses 'Map' to create multiple 'CouponState' instances from a list parameter.
-- Requires provider signature and spending a UTxO to act as the batch ID.
type BatchCreateCouponsSpec =
  'ActionSpec "BatchCreateCoupons"
    '[ -- List of ActionSteps:
       'Map -- Map over the "newCoupons" parameter list
         ('Create @CouponState -- Operation to perform for each item
            '[ 'SetTo "couponId" ('ParamValue "couponId")
             , 'SetTo "couponBatchId" ('ParamValue "batchIdUtxo") 
             , 'SetTo "couponDiscountPercent" ('ParamValue "couponDiscountPercent")
             ]
            '[])
         "newCoupons" -- Name of the list parameter (type [Coupon])
         '[ 'MustHaveUniqueField "couponId" ] -- Ensure coupon IDs are unique within this batch creation
     ]
    '[ -- Constraints:
       'MustBeSignedByState @ServiceConfigState 'TypedTheOnlyInstance "serviceConfigProvider"
     , 'MustSpendActionParam "batchIdUtxo"
     ]
    '[ -- List of action parameters:
       '("newCoupons", [Coupon])
     , '("batchIdUtxo", TxOutRef)
     ]

-- | Validated token for 'BatchCreateCouponsSpec'.
batchCreateCouponSpec :: TypedAction SubscriptionApp BatchCreateCouponsSpec
batchCreateCouponSpec = mkTypedAction (Proxy @SubscriptionApp) (Proxy @BatchCreateCouponsSpec)


-- | Specification for the "BatchDeleteCoupons" action.
-- Uses 'Map' to delete multiple 'CouponState' instances based on data from a list parameter.
-- Requires provider signature.
type BatchDeleteCouponsSpec =
  'ActionSpec "BatchDeleteCoupons"
    '[ -- List of ActionSteps:
       'Map -- Map over the "couponsToDelete" parameter list
         ('Delete @CouponState -- Operation to perform for each item
            -- Reference the specific coupon to delete using a predicate based on list item fields
            ('TypedUniqueWhere
                ('And ('FieldEquals "couponId" ('ParamValue "couponId"))
                       ('FieldEquals "couponBatchId" ('ParamValue "couponBatchId"))
                )
            )
            '[])
         "couponsToDelete" -- Name of the list parameter (type [Coupon])
         '[] -- No collection constraints needed for delete
     ]
    '[ -- Constraints:
       'MustBeSignedByState @ServiceConfigState 'TypedTheOnlyInstance "serviceConfigProvider"
     ]
    '[ -- List of action parameters:
       '("couponsToDelete", [Coupon])
     ]

-- | Validated token for 'BatchDeleteCouponsSpec'.
batchDeleteCouponsSpec :: TypedAction SubscriptionApp BatchDeleteCouponsSpec
batchDeleteCouponsSpec = mkTypedAction (Proxy @SubscriptionApp) (Proxy @BatchDeleteCouponsSpec)


-- | Specification for the "Subscribe" action (without coupon).
-- References a 'PricingTierState', creates a 'CustomerSubscriptionState',
-- calculates payment and end dates, requires customer signature, and adds payment to the treasury.
type SubscribeSpec =
  'ActionSpec "Subscribe"
    '[ -- List of ActionSteps:
       'Let "selectedTier" -- Label the referenced PricingTier for later use
         ('Reference @PricingTierState
           -- Find the unique tier matching the name provided in action parameters
           ('TypedUniqueWhere ('FieldEquals "pricingTierName" ('ParamValue "tierName")))
           '[]
         )
     , 'Op ('Create @CustomerSubscriptionState -- Create the subscription state
         '[ 'SetTo "customerSubscriptionPkh" ('ParamValue "customerPkh")
          , 'SetTo "customerSubscriptionPrice"
              ('StateFieldValue "selectedTier" "pricingTierPrice")
          , 'SetTo "customerSubscriptionAssetClass"
              ('StateFieldValue "selectedTier" "pricingTierAssetClass")
          , 'SetTo "customerSubscriptionBillingPeriod"
              ('StateFieldValue "selectedTier" "pricingTierBillingPeriod")
          , 'SetTo "customerSubscriptionPaidThrough"
              ('AddValue
                 'CurrentTime
                 ('StateFieldValue "selectedTier" "pricingTierBillingPeriod")
              )
          , 'SetTo "customerSubscriptionContractEndDate"
              ('AddValue
                 'CurrentTime
                 ('StateFieldValue "selectedTier" "pricingTierContractLength")
              )
          , 'SetTo "customerSubscriptionServiceProviderValidator"
              ('ParamValue "derivedServiceProviderValidatorHash")
          ]
         '[]
        )
     ]
    '[ -- Constraints:
       'MustBeSignedByParam "customerPkh"
     , 'MustAddToAggregateState TreasuryAdaState
          ('StateFieldValue "selectedTier" "pricingTierPrice")
     ]
    '[ -- List of action parameters:
       '("customerPkh", PubKeyHash)
     , '("tierName", BuiltinByteString)
     ]

-- | Validated token for 'SubscribeSpec'.
subscribeSpec :: TypedAction SubscriptionApp SubscribeSpec
subscribeSpec = mkTypedAction (Proxy @SubscriptionApp) (Proxy @SubscribeSpec)


-- | Specification for the "WithdrawTreasury" action.
-- Allows the service provider to withdraw funds from the 'TreasuryAdaState'.
-- Requires provider signature.
type WithdrawTreasurySpec =
  'ActionSpec "WithdrawTreasury"
    '[] -- No specific operational steps needed (handled by constraint)
    '[ -- Constraints:
       'MustBeSignedByState @ServiceConfigState 'TypedTheOnlyInstance "serviceConfigProvider"
     , 'MustWithdrawFromAggregateState TreasuryAdaState
          ('ParamValue "amount") -- Amount from action parameter
          ('ParamValue "destination") -- Destination address from action parameter
     ]
    '[ -- List of action parameters:
       '("amount", Integer)
     , '("destination", Address)
     ]

-- | Validated token for 'WithdrawTreasurySpec'.
withdrawTreasurySpec :: TypedAction SubscriptionApp WithdrawTreasurySpec
withdrawTreasurySpec = mkTypedAction (Proxy @SubscriptionApp) (Proxy @WithdrawTreasurySpec)


-- | Specification for the "SubscribeWithCoupon" action.
-- Similar to 'SubscribeSpec' but references and consumes a 'CouponState' to calculate a discounted price.
type SubscribeWithCouponSpec =
  'ActionSpec "SubscribeWithCoupon"
    '[ -- List of ActionSteps:
       'Let "selectedTier" -- Reference the chosen pricing tier
         ('Reference @PricingTierState
           ('TypedUniqueWhere ('FieldEquals "pricingTierName" ('ParamValue "tierName")))
           '[])
     , 'Let "selectedCoupon" -- Reference the specific coupon being used
         ('Reference @CouponState
           ('TypedUniqueWhere
              ('And ('FieldEquals "couponId" ('ParamValue "couponCode"))
                     ('FieldEquals "couponBatchId" ('ParamValue "couponBatch"))
              )
           )
           '[])
     , 'Op ('Create @CustomerSubscriptionState -- Create the subscription state
         '[ 'SetTo "customerSubscriptionPkh" ('ParamValue "customerPkh")
          , 'SetTo "customerSubscriptionPrice"
              ('SubtractValue
                 ('StateFieldValue "selectedTier" "pricingTierPrice")
                 ('DivideValue
                    ('MultiplyValue
                       ('StateFieldValue "selectedTier" "pricingTierPrice")
                       ('StateFieldValue "selectedCoupon" "couponDiscountPercent")
                    )
                    ('IntValue 100)
                 )
              )
          , 'SetTo "customerSubscriptionAssetClass"
              ('StateFieldValue "selectedTier" "pricingTierAssetClass")
          , 'SetTo "customerSubscriptionBillingPeriod"
              ('StateFieldValue "selectedTier" "pricingTierBillingPeriod")
          , 'SetTo "customerSubscriptionPaidThrough"
              ('AddValue
                 'CurrentTime
                 ('StateFieldValue "selectedTier" "pricingTierBillingPeriod")
              )
          , 'SetTo "customerSubscriptionContractEndDate"
              ('AddValue
                 'CurrentTime
                 ('StateFieldValue "selectedTier" "pricingTierContractLength")
              )
          , 'SetTo "customerSubscriptionServiceProviderValidator"
              ('ParamValue "derivedServiceProviderValidatorHash")
          ]
         '[])
     , 'Op ('Delete @CouponState ('TypedByLabel "selectedCoupon") '[]) -- Consume/delete the used coupon
     ]
    '[ -- Constraints:
       'MustBeSignedByParam "customerPkh"
     , 'MustAddToAggregateState TreasuryAdaState
          ('SubtractValue
             ('StateFieldValue "selectedTier" "pricingTierPrice")
             ('DivideValue
                ('MultiplyValue
                   ('StateFieldValue "selectedTier" "pricingTierPrice")
                   ('StateFieldValue "selectedCoupon" "couponDiscountPercent")
                )
                ('IntValue 100)
             )
          )
     ]
    '[ -- List of action parameters:
       '("customerPkh", PubKeyHash)
     , '("tierName", BuiltinByteString)
     , '("couponCode", Integer)
     , '("couponBatch", TxOutRef)
     ]

-- | Validated token for 'SubscribeWithCouponSpec'.
subscribeWithCouponSpec :: TypedAction SubscriptionApp SubscribeWithCouponSpec
subscribeWithCouponSpec = mkTypedAction (Proxy @SubscriptionApp) (Proxy @SubscribeWithCouponSpec)