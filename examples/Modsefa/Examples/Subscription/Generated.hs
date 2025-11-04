{-|
Module      : Modsefa.Examples.Subscription.Generated
Description : Generated Haskell code for the Subscription example application's validators.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC (requires TemplateHaskell)

This module contains Haskell code generated automatically using Template Haskell (TH)
splices provided by the Modsefa code generation system ('Modsefa.CodeGen.Generation').
It includes:

1.  The TH splice `$(generateAllValidatorsForApp @SubscriptionApp)` which generates the
    parameterized validator logic functions (e.g., `mkParameterizedServiceAndPricingValidator`) and
    their Plutus Tx wrappers (e.g., `mkWrappedParameterizedServiceAndPricingValidator`) based on the
    'Modsefa.Examples.Subscription.Spec.SubscriptionApp' specification.
2.  Functions (e.g., 'serviceAndPricingValidatorCompiledCode') that provide the 'PlutusTx.CompiledCode'
    for each validator, applying necessary parameters using 'PlutusTx.liftCode' and
    'PlutusTx.unsafeApplyCode'. These are typically used by the 'Modsefa.Examples.Subscription.Scripts'
    module to implement the 'AppValidatorScripts' instance.
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Modsefa.Examples.Subscription.Generated
  ( -- * Compiled Validator Code
    serviceAndPricingValidatorCompiledCode
  , couponValidatorCompiledCode
  , customerValidatorCompiledCode
  , treasuryValidatorCompiledCode
  ) where

import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.V3 (Address, PubKeyHash, TxOutRef)
import PlutusTx 
  ( BuiltinData, CompiledCode, ToData(toBuiltinData), compile, liftCode
  , unsafeApplyCode
  )
import PlutusTx.Prelude (BuiltinUnit)

import Modsefa.CodeGen.Generation (generateAllValidatorsForApp)

import Modsefa.Examples.Subscription.Spec (SubscriptionApp)
import Modsefa.Examples.Subscription.Types
  ( Coupon(..), CustomerSubscription(..), PricingTier(..), ServiceConfig(..)
  , TreasuryAdaState
  )


-- ============================================================================
-- Generate Validator
-- ============================================================================

-- | This Template Haskell splice invokes the Modsefa code generator.
-- It generates the parameterized logic and wrapper functions for all validators
-- defined in the 'SubscriptionApp' specification. For example:
--   - `mkParameterizedServiceAndPricingValidator :: TxOutRef -> ScriptContext -> Bool`
--   - `mkWrappedParameterizedServiceAndPricingValidator :: BuiltinData -> BuiltinData -> BuiltinUnit`
--   - `mkParameterizedCouponValidator :: Address -> ScriptContext -> Bool`
--   - `mkWrappedParameterizedCouponValidator :: BuiltinData -> BuiltinData -> BuiltinUnit`
--   - (and similarly for CustomerValidator and TreasuryValidator)
$(generateAllValidatorsForApp @SubscriptionApp)

-- ============================================================================
-- Compiled Code Functions
-- ============================================================================

-- | Provides the compiled Plutus Tx code for the 'ServiceAndPricingValidator'.
-- Applies the 'TxOutRef' parameter (for the bootstrap UTxO) to the compiled wrapper function.
serviceAndPricingValidatorCompiledCode :: TxOutRef -> CompiledCode (BuiltinData -> BuiltinUnit)
serviceAndPricingValidatorCompiledCode ref =
  $$(compile [|| mkWrappedParameterizedServiceAndPricingValidator ||])
    `unsafeApplyCode` liftCode plcVersion110 (toBuiltinData ref)

-- | Provides the compiled Plutus Tx code for the 'CouponValidator'.
-- Applies the 'Address' parameter (the address of the ServiceAndPricingValidator) to the compiled wrapper function.
couponValidatorCompiledCode :: Address -> CompiledCode (BuiltinData -> BuiltinUnit)
couponValidatorCompiledCode ref =
  $$(compile [|| mkWrappedParameterizedCouponValidator ||])
    `unsafeApplyCode` liftCode plcVersion110 (toBuiltinData ref)

-- | Provides the compiled Plutus Tx code for the 'CustomerValidator'.
-- Applies the 'PubKeyHash' parameter (the customer's PKH) to the compiled wrapper function.
customerValidatorCompiledCode :: PubKeyHash -> CompiledCode (BuiltinData -> BuiltinUnit)
customerValidatorCompiledCode pkh =
  $$(compile [|| mkWrappedParameterizedCustomerValidator ||])
    `unsafeApplyCode` liftCode plcVersion110 (toBuiltinData pkh)

-- | Provides the compiled Plutus Tx code for the 'TreasuryValidator'.
-- Applies the 'Address' parameter (the address of the ServiceAndPricingValidator) to the compiled wrapper function.
treasuryValidatorCompiledCode :: Address -> CompiledCode (BuiltinData -> BuiltinUnit)
treasuryValidatorCompiledCode ref =
  $$(compile [|| mkWrappedParameterizedTreasuryValidator ||])
    `unsafeApplyCode` liftCode plcVersion110 (toBuiltinData ref)