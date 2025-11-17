{-|
Module      : Modsefa.Examples.Feed.Generated.Datums
Description : Generates concrete datum types for the Feed example.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module's sole purpose is to run the Template Haskell
splices that generate the on-chain datum types (e.g., 'FeedConfig')
from their 'StateSpec' definitions.

It imports the 'StateSpec' instances from 'Modsefa.Examples.Feed.Types'
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Modsefa.Examples.Feed.Generated.Datums
  ( FeedConfig(..)
  , FeedData(..)
  ) where

import PlutusLedgerApi.V3 (BuiltinByteString, PubKeyHash)

import Modsefa.CodeGen.Generation (generateStateDatum, generateStateInstances)

import Modsefa.Examples.Feed.Types (FeedConfigState, FeedDataState, FeedStatus)


-- ============================================================================
-- 1. Generate Datum Types
-- ============================================================================

$(generateStateDatum @FeedConfigState)
$(generateStateDatum @FeedDataState)

-- ============================================================================
-- 2. Generate Instances
-- ============================================================================
-- We run this splice after the data types above have been created, due
-- to staging restrictions.

$(generateStateInstances)