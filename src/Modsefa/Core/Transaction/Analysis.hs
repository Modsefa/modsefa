{-|
Module      : Modsefa.Core.Transaction.Analysis
Description : Analysis functions for transaction building support.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module provides helper functions to analyze Modsefa action specifications
('SActionSpec') to support intelligent transaction construction. Currently,
it includes functionality to determine the appropriate 'RedeemerPolicy' for an action.
-}
{-# LANGUAGE GADTs #-}
-- | Analysis functions for transaction building
--
-- This module provides helper functions to analyze action specifications
-- to support intelligent transaction construction, such as determining
-- redeemer policies.
module Modsefa.Core.Transaction.Analysis
  ( determineRedeemerPolicy
  ) where

import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import GHC.TypeLits (symbolVal)

import PlutusLedgerApi.V3 (toBuiltin)

import Modsefa.Core.Singletons (SAppSpec, SActionSpec(..))

import Modsefa.Core.Transaction.Types (RedeemerPolicy(..))


-- | Determines the 'RedeemerPolicy' to be used when building a transaction for a given action.
--
-- In Modsefa, each action handled by a specific validator corresponds to a unique branch
-- in the validator's logic, identified by the action's name. Therefore, this function
-- always returns a 'UseNamedRedeemer' policy, using the action's name (extracted from
-- the 'SActionSpec' and converted to 'BuiltinByteString') as the redeemer value.
determineRedeemerPolicy :: SAppSpec app -- ^ The application specification singleton (currently unused, but kept for potential future context).
                        -> SActionSpec app action -- ^ The singleton representing the action specification.
                        -> RedeemerPolicy -- ^ The resulting redeemer policy.
determineRedeemerPolicy _appSpec' (SActionSpec nameProxy _steps _constraints _) =
  let
    -- Extract the action name from the type-level Symbol via the Proxy
    actionName = pack $ symbolVal nameProxy
  in
    -- Convert the Text actionName to a BuiltinByteString for the redeemer
    UseNamedRedeemer (toBuiltin $ encodeUtf8 actionName)