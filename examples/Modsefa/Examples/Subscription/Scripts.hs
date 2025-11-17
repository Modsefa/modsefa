{-|
Module      : Modsefa.Examples.Subscription.Scripts
Description : Provides the AppValidatorScripts instance for the Subscription example application.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module defines the 'AppValidatorScripts' instance for the 'SubscriptionApp'.
This instance implements the 'getValidatorScript' method, which acts as a dispatcher.
Based on the provided validator type ('Proxy v'), it selects the appropriate
compiled Plutus script (e.g., 'serviceAndPricingValidatorCompiledCode' from
'Modsefa.Examples.Subscription.Generated') and applies the resolved validator parameters
to it, returning the final 'GYScript' ready for use in transaction building.
-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Modsefa.Examples.Subscription.Scripts () where

import Data.Typeable (eqT)

import GeniusYield.Types (scriptFromPlutus)

import Modsefa.CodeGen.Generation (generateAppValidatorScripts)

import Modsefa.Examples.Subscription.Generated 
  ( couponValidatorCompiledCode, customerValidatorCompiledCode
  , serviceAndPricingValidatorCompiledCode, treasuryValidatorCompiledCode
  )
import Modsefa.Examples.Subscription.Spec (SubscriptionApp)
import Modsefa.Examples.Subscription.Validators
  ( CouponValidator, CustomerValidator, ServiceAndPricingValidator
  , TreasuryValidator 
  )


-- ============================================================================
-- AppValidatorScripts Instance
-- ============================================================================

$(generateAppValidatorScripts @SubscriptionApp)