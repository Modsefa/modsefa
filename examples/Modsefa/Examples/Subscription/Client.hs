{-|
Module      : Modsefa.Examples.Subscription.Client
Description : Client-side setup functions for the Subscription example application.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module provides convenience functions for creating and working with
application-specific types for the 'SubscriptionApp'. Its main export,
'mkSubscriptionAppInstance', simplifies the creation of the
'SAppInstance SubscriptionApp' from a bootstrap 'TxOutRef', which is
required by client-side functions like 'runAction' and 'queryStateInstances'.
-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Modsefa.Examples.Subscription.Client
  ( -- * Instance Creation
    mkSubscriptionAppInstance
    -- * Action Execution
    --, initializeSubscriptionService_
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

import Modsefa.Examples.Subscription.Scripts ()
import Modsefa.Examples.Subscription.Spec (SubscriptionApp)


-- ============================================================================
-- * Instance Creation
-- ============================================================================

-- | Creates an SAppInstance for the SubscriptionApp from a Tx hash string and index.
--   Returns Left with an error message if the hash format is invalid.
mkSubscriptionAppInstance
  :: String             -- ^ Transaction hash hex string for the bootstrap UTxO.
  -> Integer            -- ^ Output index for the bootstrap UTxO.
  -> Either Text (SAppInstance SubscriptionApp) -- ^ Error or the SubscriptionApp instance.
mkSubscriptionAppInstance txHashStr txIndex =
  case txIdFromHex txHashStr of
    Nothing -> Left $ pack $ "Invalid transaction hash format: " ++ txHashStr
    Just _ ->
      let bootstrapRef = TxOutRef (fromString txHashStr) txIndex
      in Right $ createAppInstance' @SubscriptionApp bootstrapRef

-- ============================================================================
-- * Helpers
-- ============================================================================

-- | Helper to extract bootstrap ref string for logging.
appInstanceBootstrapRef :: SAppInstance SubscriptionApp -> String
appInstanceBootstrapRef (SAppInstance _ (SInstanceParams ref)) = show (ref :: TxOutRef)

-- ============================================================================
-- * Example Wrapper Pattern (Commented Out)
-- ============================================================================

{-
-- | (EXAMPLE) Initializes a new Subscription service instance using the PubKeyHash derived from
--   the 'ModsefaClient's signer as the service provider.
initializeSubscriptionService_
  :: ModsefaClient       -- ^ The client session handle (must contain the provider's key).
  -> SAppInstance SubscriptionApp -- ^ The application instance (defines the bootstrap UTxO).
  -> BuiltinByteString   -- ^ serviceName
  -> BuiltinByteString   -- ^ initial tierName
  -> Integer             -- ^ initial price
  -> AssetClass          -- ^ initial assetClass
  -> POSIXTime           -- ^ initial billingPeriod
  -> POSIXTime           -- ^ initial contractLength
  -> IO (Either Text GYTxId) -- ^ Result of the transaction submission.
initializeSubscriptionService_ client appInstance serviceName tierName price assetClass billingPeriod contractLength = do
  putStrLn "Initializing Subscription service using client's signer as provider..."
  -- Get the provider PKH from the client handle
  case mcSigners client of
    (providerPkh, _) : _ -> do
      -- Define parameters as a flat tuple
      let paramsTuple = (serviceName, providerPkh, tierName, price, assetClass, billingPeriod, contractLength)
      -- Call the generic runAction
      runAction client appInstance (Proxy @InitializeServiceSpec) paramsTuple
    [] -> return $ Left "ModsefaClient has no signers to use as provider."
-}