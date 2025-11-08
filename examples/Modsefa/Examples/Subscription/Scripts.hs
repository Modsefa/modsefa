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

import Data.Data ((:~:)(Refl))
import Data.Proxy (Proxy)
import Data.Typeable (Typeable, eqT)

import GeniusYield.Types (GYScript, scriptFromPlutus)

import Modsefa.Core.Foundation
  ( Params, ParamsToValue, ValidatorPlutusVersion
  , ValidatorSpec
  )
import Modsefa.Core.ValidatorScript (AppValidatorScripts(getValidatorScript))

import Modsefa.Examples.Subscription.Generated 
  ( couponValidatorCompiledCode
  , customerValidatorCompiledCode
  , serviceAndPricingValidatorCompiledCode
  , treasuryValidatorCompiledCode
  )
import Modsefa.Examples.Subscription.Spec (SubscriptionApp)
import Modsefa.Examples.Subscription.Validators
  ( CouponValidator, CustomerValidator, ServiceAndPricingValidator
  , TreasuryValidator 
  )


-- ============================================================================
-- AppValidatorScripts Instance
-- ============================================================================

-- | Instance connecting the 'SubscriptionApp' specification to its concrete validator script implementations.
instance AppValidatorScripts SubscriptionApp where
  -- | Retrieves the parameterized 'GYScript' for a given validator type 'v' within the 'SubscriptionApp'.
  -- This function uses 'eqT' for type-safe dispatching at runtime based on the 'Proxy v'.
  -- It looks up the correct compiled code function (e.g., 'serviceAndPricingValidatorCompiledCode')
  -- and applies the provided 'params' ('ParamsToValue (Params v)') to it.
  getValidatorScript :: forall v. (ValidatorSpec v, Typeable v) =>
    Proxy v -- ^ Proxy identifying the requested validator type.
    -> ParamsToValue (Params v) -- ^ The resolved value-level parameters for this validator instance.
    -> GYScript (ValidatorPlutusVersion v) -- ^ The resulting parameterized Genius Yield script.
  getValidatorScript _ params =
    -- Check if the requested type 'v' matches ServiceAndPricingValidator
    case eqT @v @ServiceAndPricingValidator of
      Just Refl -> scriptFromPlutus $ serviceAndPricingValidatorCompiledCode params
      Nothing ->
        -- Check if 'v' matches CouponValidator
        case eqT @v @CouponValidator of
          Just Refl -> scriptFromPlutus $ couponValidatorCompiledCode params
          Nothing ->
            -- Check if 'v' matches CustomerValidator
            case eqT @v @CustomerValidator of
              Just Refl -> scriptFromPlutus $ customerValidatorCompiledCode params
              Nothing ->
                -- Check if 'v' matches TreasuryValidator
                case eqT @v @TreasuryValidator of
                  Just Refl -> scriptFromPlutus $ treasuryValidatorCompiledCode params
                  -- If no match is found for any known validator in this app, raise an error.
                  Nothing -> error "No script implementation for this validator"