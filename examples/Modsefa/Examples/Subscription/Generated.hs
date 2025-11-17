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

It includes a single TH splice, `$(generateAppCode @SubscriptionApp)`, which is responsible
for generating all of the following based on the 'SubscriptionApp'
specification:

1.  Parameterized validator logic functions (e.g., `mkParameterizedServiceAndPricingValidator`).
2.  Plutus Tx wrapper functions (e.g., `mkWrappedParameterizedServiceAndPricingValidator`).
3.  Compiled code functions (e.g., 'serviceAndPricingValidatorCompiledCode') that provide the
    'PlutusTx.CompiledCode' for each validator. These are used by the
    'Modsefa.Examples.Subscription.Scripts' module to implement the
    'AppValidatorScripts' instance.
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

import PlutusLedgerApi.V3 (Address, PubKeyHash, TxOutRef)

import Modsefa.CodeGen.Generation (generateAppCode)

import Modsefa.Examples.Subscription.Generated.Datums
  ( Coupon(..), CustomerSubscription(..), PricingTier(..), ServiceConfig(..)
  )
import Modsefa.Examples.Subscription.Spec (SubscriptionApp)


-- ============================================================================
-- Generate Validator and Compiled Code Functions
-- ============================================================================

$(generateAppCode @SubscriptionApp)