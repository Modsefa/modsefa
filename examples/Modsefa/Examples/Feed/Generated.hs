{-|
Module      : Modsefa.Examples.Feed.Generated
Description : Generated Haskell code for the Feed example application's validators.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC (requires TemplateHaskell)

This module contains Haskell code generated automatically using Template Haskell (TH)
splices provided by the Modsefa code generation system ('Modsefa.CodeGen.Generation').
It includes:

1.  The TH splice `$(generateAllValidatorsForApp @FeedApp)` which generates the
    parameterized validator logic function (e.g., `mkParameterizedFeedValidator`) and
    its Plutus Tx wrapper (e.g., `mkWrappedParameterizedFeedValidator`) based on the
    'Modsefa.Examples.Feed.Spec.FeedApp' specification.
2.  Functions (e.g., 'feedValidatorCompiledCode') that provide the 'PlutusTx.CompiledCode'
    for the validator, applying necessary parameters using 'PlutusTx.liftCode' and
    'PlutusTx.unsafeApplyCode'. These are typically used by the 'Modsefa.Examples.Feed.Scripts'
    module to implement the 'Modsefa.Core.ValidatorScript.AppValidatorScripts' instance.
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Modsefa.Examples.Feed.Generated
  ( -- * Compiled Validator Code
    feedValidatorCompiledCode
  ) where

import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.V3 (BuiltinData, ToData(toBuiltinData), TxOutRef)
import PlutusTx (CompiledCode, compile, liftCode, unsafeApplyCode)
import PlutusTx.Prelude (BuiltinUnit)

import Modsefa.CodeGen.Generation (generateAllValidatorsForApp)

import Modsefa.Examples.Feed.Spec (FeedApp)
import Modsefa.Examples.Feed.Types (FeedConfig(..), FeedData(..), FeedStatus(..))


-- ============================================================================
-- Generate Validator
-- ============================================================================

-- | This Template Haskell splice invokes the Modsefa code generator.
-- It generates the following functions based on the 'FeedApp' specification:
--   - `mkParameterizedFeedValidator :: TxOutRef -> ScriptContext -> Bool` (The core logic)
--   - `mkWrappedParameterizedFeedValidator :: BuiltinData -> BuiltinData -> BuiltinUnit` (The Plutus Tx wrapper)
$(generateAllValidatorsForApp @FeedApp)

-- ============================================================================
-- Compiled Code Functions
-- ============================================================================

-- | Provides the compiled Plutus Tx code for the 'FeedValidator'.
-- It takes the validator's runtime parameter ('TxOutRef' for the bootstrap UTxO),
-- lifts it to Plutus 'BuiltinData', applies it to the compiled wrapper function,
-- and returns the resulting 'CompiledCode' ready to be used for script creation.
feedValidatorCompiledCode :: TxOutRef -> CompiledCode (BuiltinData -> BuiltinUnit)
feedValidatorCompiledCode ref =
  $$(compile [|| mkWrappedParameterizedFeedValidator ||])
    `unsafeApplyCode` liftCode plcVersion110 (toBuiltinData ref)