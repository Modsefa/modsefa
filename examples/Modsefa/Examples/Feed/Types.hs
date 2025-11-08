{-|
Module      : Modsefa.Examples.Feed.Types
Description : Defines the data types and state types for the simple Feed example application.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module defines:
1.  The Haskell records ('FeedConfig', 'FeedData') used as datum types on-chain.
2.  An enumeration ('FeedStatus') used within 'FeedData'.
3.  The corresponding Modsefa 'Modsefa.Core.Foundation.Types.StateType' aliases ('FeedConfigState', 'FeedDataState').
4.  'Modsefa.Core.Foundation.Types.StateRepresentable' instances defining how these states are identified and referenced on-chain.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Modsefa.Examples.Feed.Types
  ( -- * Data Types
    FeedConfig (..)
  , FeedData (..)
  , FeedStatus (..)
    -- * State Type Aliases
  , FeedConfigState
  , FeedDataState
  ) where

import GHC.Generics (Generic)

import PlutusLedgerApi.Common (BuiltinByteString)
import PlutusLedgerApi.V3 (PubKeyHash)
import PlutusTx.Builtins (equalsData)
import qualified PlutusTx
import qualified PlutusTx.Eq

import Modsefa.Core.Foundation
  (PolicySource (OwnPolicy), RefStrategy (..), StateIdentifier (TokenIdentified)
  , StateRepresentable (..), StateType (..)
  )
import Modsefa.Core.Singletons (FromEnumValue)
import Modsefa.Core.Transaction (Mappable)


-- ============================================================================
-- 1. Data Types (Used as On-Chain Datums)
-- ============================================================================

-- | Configuration data for the feed. Expected to be a singleton state.
data FeedConfig = FeedConfig
  { feedName :: BuiltinByteString -- ^ The display name of the feed.
  , feedOwner :: PubKeyHash -- ^ The public key hash authorized to update the feed.
  } deriving (Eq, Generic, Show)
PlutusTx.makeLift ''FeedConfig
PlutusTx.makeIsDataIndexed ''FeedConfig [('FeedConfig, 0)]

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

-- | Represents a single data entry in the feed. Multiple instances can exist (active/archived).
data FeedData = FeedData
  { feedData :: BuiltinByteString -- ^ The content of the feed entry.
  , feedStatus :: FeedStatus  -- ^ The status ('Active' or 'Archived').
  } deriving (Eq, Generic, Show)
PlutusTx.makeLift ''FeedData
PlutusTx.makeIsDataIndexed ''FeedData [('FeedData, 0)]
-- Provide Mappable instance if needed for batch operations (though not used in Feed spec).
instance Mappable FeedData

-- ============================================================================
-- 2. StateType Definitions (Type Aliases)
-- ============================================================================

-- | Modsefa 'StateType' alias connecting the name "FeedConfig" to the 'FeedConfig' data type.
type FeedConfigState = 'ST "FeedConfig" FeedConfig
-- | Modsefa 'StateType' alias connecting the name "FeedData" to the 'FeedData' data type.
type FeedDataState = 'ST "FeedData" FeedData

-- ============================================================================
-- 3. StateRepresentable Instances
-- ============================================================================

-- | Defines how 'FeedDataState' is represented on-chain.
instance StateRepresentable FeedDataState where
  -- | Identified by a unique token named "FeedData" (quantity 1) minted using the validator's own script hash ('OwnPolicy').
  stateIdentifier _ = TokenIdentified OwnPolicy "FeedData" 1
  -- | Can only be referenced using predicates ('TypedUniqueWhere', 'TypedAnyWhere').
  type AllowedRefStrategy FeedDataState = 'OnlyByProperty

-- | Defines how 'FeedConfigState' is represented on-chain.
instance StateRepresentable FeedConfigState where
  -- | Identified by a unique token named "FeedConfig" (quantity 1) minted using the validator's own script hash ('OwnPolicy').
  stateIdentifier _ = TokenIdentified OwnPolicy "FeedConfig" 1
  -- | Can only be referenced as the single unique instance ('TypedTheOnlyInstance').
  type AllowedRefStrategy FeedConfigState = 'OnlyAsUnique