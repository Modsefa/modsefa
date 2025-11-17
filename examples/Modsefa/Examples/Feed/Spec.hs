{-|
Module      : Modsefa.Examples.Feed.Spec
Description : Defines the application and action specifications for the Feed example.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module defines the core Modsefa specification for a simple "Feed" application.
It includes:
1.  The 'FeedApp' type tag.
2.  The 'AppSpec' instance for 'FeedApp', which declares the application's validators, states, transitions, and parameters.
3.  Type aliases ('InitializeFeedSpec', 'UpdateFeedSpec') for the 'TypedActionSpec's defining the application's actions.
4.  Validated 'TypedAction' values ('initializeFeedSpec', 'updateFeedSpec') for each action specification, ensuring they are well-formed at compile time.
-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Modsefa.Examples.Feed.Spec
  ( -- * Application Type Tag
    FeedApp
    -- * Action Specification Types
  , InitializeFeedSpec
  , UpdateFeedSpec
    -- * Validated Action Tokens
  , initializeFeedSpec
  , updateFeedSpec
  ) where

import Data.Proxy (Proxy(Proxy))

import PlutusLedgerApi.V3 (BuiltinByteString, PubKeyHash)

import Modsefa.Core.Actions (TypedAction, mkTypedAction)
import Modsefa.Core.Foundation
  ( ActionStep(Op), AppSpec(..), FieldSpec(Preserve, SetTo)
  , TypedActionSpec(ActionSpec)
  , TypedConstraint(MustBeSignedByState, MustSpendValidatorParam)
  , TypedOperation(Create, Update), TypedPredicate(FieldEquals)
  , TypedStateRef(TypedTheOnlyInstance, TypedUniqueWhere), TypedValue(..)
  , ValidatorDef(Validator)
  )

import Modsefa.Examples.Feed.Types (FeedConfigState, FeedDataState, FeedStatus)
import Modsefa.Examples.Feed.Validators (FeedValidator)


-- ============================================================================
-- Application Specification
-- ============================================================================

-- | A type-level tag representing the Feed application. Used as the @app@ type argument
-- in 'AppSpec', 'TypedActionSpec', etc.
data FeedApp

-- | Defines the overall structure and rules of the 'FeedApp'.
instance AppSpec FeedApp where
  -- | Declares the validators used by this application. Here, only 'FeedValidator'.
  type Validators FeedApp =
    '[ 'Validator FeedValidator
     ]
  -- | Defines the high-level application states for state machine tracking.
  type AppStates FeedApp = '["Uninitialized", "Initialized"]
  -- | Specifies the initial state of the application state machine.
  type InitialAppState FeedApp = "Uninitialized"
  -- | Defines the allowed transitions between 'AppStates' triggered by specific actions.
  type ActionTransitions FeedApp =
    '[ '(InitializeFeedSpec, "Uninitialized", "Initialized")
     , '(UpdateFeedSpec, "Initialized", "Initialized")
     ]
  -- | Specifies the instance parameters required when instantiating this application.
  -- Links the 'FeedValidator' (identified by its 'ValidatorAppName' "FeedValidator")
  -- to an instance parameter named "bootstrapUtxo".
  type AppInstanceParameters FeedApp =
    '[ '("FeedValidator", "bootstrapUtxo")
     ]
  -- | Specifies any automatic parameter derivation rules. None for this simple example.
  type ParameterDerivations FeedApp = '[]

-- ============================================================================
-- Action Specifications
-- ============================================================================

-- | Type alias for the 'TypedActionSpec' defining the "InitializeFeed" action.
-- This action creates the initial 'FeedConfigState' and 'FeedDataState' instances.
type InitializeFeedSpec =
  'ActionSpec "InitializeFeed"
    '[ -- List of ActionSteps:
       'Op ('Create @FeedConfigState -- Create the config state
         '[ 'SetTo "feedName" ('ParamValue "name") -- Set field 'feedName' from action param "name"
          , 'SetTo "feedOwner" ('ParamValue "owner") -- Set field 'feedOwner' from action param "owner"
          ]
         '[]) -- No operation-specific constraints
     , 'Op ('Create @FeedDataState -- Create the initial data state
         '[ 'SetTo "feedData" ('ParamValue "content") -- Set field 'feedData' from action param "content"
          , 'SetTo "feedStatus" ('EnumValue FeedStatus "Active") -- Set field 'feedStatus' to the literal Active
          ]
         '[]) -- No operation-specific constraints
     ]
    '[ -- List of transaction-wide constraints:
       'MustSpendValidatorParam "FeedValidator" "bootstrapUtxo"
     ]
    '[ -- List of action parameters:
       '("name", BuiltinByteString)
     , '("owner", PubKeyHash)
     , '("content", BuiltinByteString)
     ]

-- | A value-level "proof token" representing the validated 'InitializeFeedSpec'.
-- 'mkTypedAction' ensures the specification is well-formed according to Modsefa's rules
-- at compile time.
initializeFeedSpec :: TypedAction FeedApp InitializeFeedSpec
initializeFeedSpec = mkTypedAction (Proxy @FeedApp) (Proxy @InitializeFeedSpec)

-- | Type alias for the 'TypedActionSpec' defining the "UpdateFeed" action.
-- This action creates a new 'Active' 'FeedDataState' and archives the previous 'Active' one.
type UpdateFeedSpec =
  'ActionSpec "UpdateFeed"
    '[ -- List of ActionSteps:
       'Op ('Create @FeedDataState -- Create the new active data entry
         '[ 'SetTo "feedData" ('ParamValue "newContent") -- Set content from action parameter
          , 'SetTo "feedStatus" ('EnumValue FeedStatus "Active") -- Set status to Active
          ]
         '[]) -- No operation-specific constraints
     , 'Op ('Update @FeedDataState -- Find and update the old active entry
          -- Reference the unique FeedData entry where feedStatus is Active
          ('TypedUniqueWhere ('FieldEquals "feedStatus" ('EnumValue FeedStatus "Active")))
          '[ 'Preserve "feedData" -- Keep the old content
           , 'SetTo "feedStatus" ('EnumValue FeedStatus "Archived") -- Change status to Archived
           ]
          '[]) -- No operation-specific constraints
     ]
    '[ -- List of transaction-wide constraints:
       -- Requires signature from the owner stored in the unique FeedConfigState
       'MustBeSignedByState @FeedConfigState 'TypedTheOnlyInstance "feedOwner"
     ]
    '[ -- List of action parameters:
       '("newContent", BuiltinByteString)
     ]

-- | A value-level "proof token" representing the validated 'UpdateFeedSpec'.
updateFeedSpec :: TypedAction FeedApp UpdateFeedSpec
updateFeedSpec = mkTypedAction (Proxy @FeedApp) (Proxy @UpdateFeedSpec)