{-|
Module      : Modsefa.Examples.Feed.Scripts
Description : Provides the AppValidatorScripts instance for the Feed example application.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module defines the crucial 'AppValidatorScripts' instance for 
the 'Modsefa.Examples.Feed.Spec.FeedApp'. This instance implements the
'getValidatorScript' method, which acts as a dispatcher.
Based on the provided validator type ('Proxy v'), it selects the appropriate
compiled Plutus script (e.g., 'feedValidatorCompiledCode' from
'Modsefa.Examples.Feed.Generated') and applies the resolved validator parameters
to it, returning the final 'GYScript' ready for use in transaction building.
-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Modsefa.Examples.Feed.Scripts () where

import Data.Data ((:~:)(Refl))
import Data.Proxy (Proxy)
import Data.Typeable (Typeable, eqT)

import GeniusYield.Types (GYScript, scriptFromPlutus)

import Modsefa.Core.Foundation 
  ( Params, ParamsToValue, ValidatorPlutusVersion, ValidatorSpec
  )
import Modsefa.Core.ValidatorScript (AppValidatorScripts(getValidatorScript))

import Modsefa.Examples.Feed.Generated (feedValidatorCompiledCode)
import Modsefa.Examples.Feed.Spec (FeedApp)
import Modsefa.Examples.Feed.Validators (FeedValidator)


-- ============================================================================
-- AppValidatorScripts Instance
-- ============================================================================

-- | Instance connecting the 'FeedApp' specification to its concrete validator script implementations.
instance AppValidatorScripts FeedApp where
  -- | Retrieves the parameterized 'GYScript' for a given validator type 'v' within the 'FeedApp'.
  -- This function uses 'eqT' for type-safe dispatching at runtime based on the 'Proxy v'.
  -- It looks up the correct compiled code function (e.g., 'feedValidatorCompiledCode')
  -- and applies the provided 'params' ('ParamsToValue (Params v)') to it.
  getValidatorScript :: forall v. (ValidatorSpec v, Typeable v) =>
    Proxy v -- ^ Proxy identifying the requested validator type (e.g., @Proxy \@FeedValidator@).
    -> ParamsToValue (Params v) -- ^ The resolved value-level parameters for this validator instance.
    -> GYScript (ValidatorPlutusVersion v) -- ^ The resulting parameterized Genius Yield script.
  getValidatorScript _ params =
    -- Check if the requested validator type 'v' is FeedValidator
    case eqT @v @FeedValidator of
      -- If yes, apply the params to the compiled code for FeedValidator.
      Just Refl -> scriptFromPlutus $ feedValidatorCompiledCode params
      -- If no match is found for any known validator in this app, raise an error.
      Nothing -> error "No script implementation for this validator."