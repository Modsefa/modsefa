{-|
Module      : Modsefa.Examples.Feed.Scripts
Description : Provides the AppValidatorScripts instance for the Feed example application.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module defines the 'AppValidatorScripts' instance for the 'FeedApp'.

It uses the 'generateAppValidatorScripts' Template Haskell splice, which
automatically generates the implementation for 'getValidatorScript'. This
generated implementation acts as a dispatcher that, based on the validator
type, selects the correct compiled Plutus script (from
'Modsefa.Examples.Feed.Generated') and applies the resolved validator
parameters to it, returning the final 'GYScript' ready for use.
-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Modsefa.Examples.Feed.Scripts () where

import Data.Typeable (eqT)

import GeniusYield.Types (scriptFromPlutus)

import Modsefa.CodeGen.Generation (generateAppValidatorScripts)

import Modsefa.Examples.Feed.Generated (feedValidatorCompiledCode)
import Modsefa.Examples.Feed.Spec (FeedApp)
import Modsefa.Examples.Feed.Validators (FeedValidator)


-- ============================================================================
-- AppValidatorScripts Instance
-- ============================================================================

$(generateAppValidatorScripts @FeedApp)