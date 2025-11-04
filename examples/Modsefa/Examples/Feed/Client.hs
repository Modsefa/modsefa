{-|
Module      : Modsefa.Examples.Feed.Client
Description : Client-side setup functions for the Feed example application.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module provides convenience functions for creating and working with
application-specific types for the 'FeedApp'. Its main export,
'mkFeedAppInstance', simplifies the creation of the 'SAppInstance FeedApp'
from a bootstrap 'TxOutRef', which is required by client-side
functions like 'runAction' and 'queryStateInstances'.
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Modsefa.Examples.Feed.Client
  ( -- * Instance Creation
    mkFeedAppInstance
   -- * Action Execution
   --, initializeFeed_
   -- * Helpers
 , appInstanceBootstrapRef
 ) where

import Data.String (fromString)
import Data.Text (Text, pack)

import GeniusYield.Types (txIdFromHex)
import PlutusLedgerApi.V3 (TxOutRef(TxOutRef))

import Modsefa.Client (createAppInstance')
import Modsefa.Core.Singletons
  ( SAppInstance(SAppInstance), SInstanceParams(SInstanceParams)
  )

import Modsefa.Examples.Feed.Scripts ()
import Modsefa.Examples.Feed.Spec (FeedApp)


-- ============================================================================
-- * Instance Creation
-- ============================================================================

-- | Creates an SAppInstance for the FeedApp from a Tx hash string and index.
--   Returns Left with an error message if the hash format is invalid.
mkFeedAppInstance
  :: String             -- ^ Transaction hash hex string for the bootstrap UTxO.
  -> Integer            -- ^ Output index for the bootstrap UTxO.
  -> Either Text (SAppInstance FeedApp) -- ^ Error or the FeedApp instance.
mkFeedAppInstance txHashStr txIndex =
  case txIdFromHex txHashStr of
    Nothing -> Left $ pack $ "Invalid transaction hash format: " ++ txHashStr
    Just _ ->
      let bootstrapRef = TxOutRef (fromString txHashStr) txIndex
      in Right $ createAppInstance' @FeedApp bootstrapRef

-- ============================================================================
-- * Helpers
-- ============================================================================

-- | Helper to extract bootstrap ref for logging
appInstanceBootstrapRef :: SAppInstance FeedApp -> String
appInstanceBootstrapRef (SAppInstance _ (SInstanceParams ref)) = show (ref :: TxOutRef)

-- ============================================================================
-- * Example Wrapper Pattern (Commented Out)
-- ============================================================================

{-
-- | (EXAMPLE) Initializes a new Feed instance using the PubKeyHash derived from
--   the 'ModsefaClient's signer.
--
--   This is a convenience function that wraps the generic 'runAction'.
initializeFeed_
  :: ModsefaClient       -- ^ The client session handle (must contain the owner's key).
  -> SAppInstance FeedApp  -- ^ The application instance (defines the bootstrap UTxO).
  -> BuiltinByteString   -- ^ The desired name for the feed.
  -> BuiltinByteString   -- ^ The initial content for the feed.
  -> IO (Either Text GYTxId) -- ^ The result of the transaction submission.
initializeFeed_ client appInstance feedName initialContent = do
  putStrLn "Initializing Feed instance using client's signer as owner..."

  -- Extract the owner PKH from the client handle.
  case mcSigners client of
    (ownerPkh, _) : _ -> do
      -- Define parameters as a flat tuple
      let paramsTuple = (feedName, ownerPkh, initialContent)
      -- Call the generic runAction
      runAction client appInstance (Proxy @InitializeFeedSpec) paramsTuple
    [] -> return $ Left "ModsefaClient has no signers to use as owner."
-}