{-|
Module      : Modsefa.Examples.Feed.Types
Description : Defines the data types and state types for the simple Feed example application.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module defines:
1.  An enumeration ('FeedStatus') used within 'FeedData'.
2.  The type-level state tags ('FeedConfigState', 'FeedDataState').
3.  'StateSpec' instances that define the on-chain properties (fields, identifier, strategy) for each state.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Modsefa.Examples.Feed.Types
  ( -- * Data Types
    FeedStatus (..)
    -- * State Type Aliases
  , FeedConfigState
  , FeedDataState
  ) where

import GHC.Generics (Generic)

import PlutusLedgerApi.V3 (BuiltinByteString, PubKeyHash)
import PlutusTx.Builtins (equalsData)
import qualified PlutusTx
import qualified PlutusTx.Eq

import Modsefa.Core.Foundation
  ( RefStrategy(..), SpecPolicySource(OwnPolicySpec)
  , SpecStateIdentifier(TokenIdentifiedSpec), StateSpec(..)
  )
import Modsefa.Core.Singletons (FromEnumValue)


-- ============================================================================
-- 1. Data Types
-- ============================================================================

-- | Status of a feed entry (active or archived).
data FeedStatus = Archived | Active
  deriving (Eq, Generic, Show)
PlutusTx.makeLift ''FeedStatus
PlutusTx.makeIsDataIndexed ''FeedStatus [('Archived, 0), ('Active, 1)]
-- Provide an instance for resolving 'EnumValue' in specifications.
instance FromEnumValue FeedStatus
-- Provide an on-chain Eq instance using PlutusTx's Data comparison.
instance PlutusTx.Eq.Eq FeedStatus where
  (==) a b = equalsData (PlutusTx.toBuiltinData a) (PlutusTx.toBuiltinData b)

-- ============================================================================
-- 2. State Tags
-- ============================================================================

-- | Type-level tag for the feed's singleton configuration state.
data FeedConfigState
-- | Type-level tag for the feed's data entries (of which there can be many).
data FeedDataState

-- ============================================================================
-- 3. StateSpec Instances
-- ============================================================================

-- | Specification for the 'FeedConfigState'.
instance StateSpec FeedConfigState where
  type DatumName FeedConfigState = "FeedConfig"
  type DatumFields FeedConfigState = 
    '[ '("feedName", BuiltinByteString)
     , '("feedOwner", PubKeyHash) 
     ]
  -- | Identified by a unique "FeedConfig" token minted by its own validator.
  type Identifier FeedConfigState = 'TokenIdentifiedSpec 'OwnPolicySpec "FeedConfig" 1
  -- | This is a singleton state; it can only be referenced as 'TypedTheOnlyInstance'.
  type Strategy FeedConfigState = 'OnlyAsUnique

instance StateSpec FeedDataState where
  type DatumName FeedDataState = "FeedData"
  type DatumFields FeedDataState = 
    '[ '("feedData", BuiltinByteString)
     , '("feedStatus", FeedStatus) 
     ]
  -- | Identified by a unique "FeedData" token minted by its own validator.
  type Identifier FeedDataState = 'TokenIdentifiedSpec 'OwnPolicySpec "FeedData" 1
  -- | Multiple instances can exist; they must be referenced by their properties
  -- | (e.g., finding the unique one where "feedStatus" is "Active").
  type Strategy FeedDataState = 'OnlyByProperty
