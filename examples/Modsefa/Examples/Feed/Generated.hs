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

This module contains Haskell code generated automatically using Template Haskell (TH)
splices provided by the Modsefa code generation system ('Modsefa.CodeGen.Generation').

It includes a single TH splice, `$(generateAppCode @FeedApp)`, which is responsible
for generating all of the following based on the 'FeedApp' specification:

1.  Parameterized validator logic functions (e.g., `mkParameterizedFeedValidator`).
2.  Plutus Tx wrapper functions (e.g., `mkWrappedParameterizedFeedValidator`).
3.  Compiled code functions (e.g., 'feedValidatorCompiledCode') that provide the
    'PlutusTx.CompiledCode' for the validator. These are used by the
    'Modsefa.Examples.Feed.Scripts' module to implement the
    'Modsefa.Core.ValidatorScript.AppValidatorScripts' instance.
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Modsefa.Examples.Feed.Generated
  ( -- * Compiled Validator Code
    feedValidatorCompiledCode
  ) where

import PlutusLedgerApi.V3 (TxOutRef)

import Modsefa.CodeGen.Generation (generateAppCode)

import Modsefa.Examples.Feed.Generated.Datums (FeedConfig(..), FeedData(..))
import Modsefa.Examples.Feed.Spec (FeedApp)
import Modsefa.Examples.Feed.Types (FeedStatus(..))


-- ============================================================================
-- Generate Validator and Compiled Code Functions
-- ============================================================================

$(generateAppCode @FeedApp)