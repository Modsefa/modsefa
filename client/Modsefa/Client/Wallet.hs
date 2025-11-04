{-|
Module      : Modsefa.Client.Wallet
Description : Defines wallet and signing abstractions for the Modsefa client.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC (requires GADTs, TypeApplications)

This module provides the core abstractions for handling user wallets and
signing methods. This allows client-side code (like test runners or web
backends) to remain flexible and support multiple users (e.g., "alice.skey",
"bob.skey") and multiple signing mechanisms (e.g., key files, browser wallets).
-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}

module Modsefa.Client.Wallet
  ( SigningMethod(..)
  , Wallet(..)
  , loadWalletAndSigner
  ) where

import GeniusYield.Types
  ( AsPubKeyHash(toPubKeyHash), GYAddress, GYKeyRole(GYKeyRolePayment), GYNetworkId
  , GYSigningKey, GYTxOutRef, addressFromPaymentKeyHash, getVerificationKey
  , pubKeyHashToPlutus, readSigningKey, verificationKeyHash
  )
import PlutusLedgerApi.V3 (PubKeyHash)


-- | Represents the abstract method by which a transaction can be signed.
--   This allows for flexibility in supporting key files, browser wallets, etc.
data SigningMethod where
  -- | The signature is provided by a local 'GYSigningKey'.
  SignWithSKey :: GYSigningKey 'GYKeyRolePayment -> SigningMethod
  --
  -- Placeholders for future signing methods:
  -- SignViaBrowser :: BrowserHandle -> SigningMethod
  -- SignWithHardware :: DeviceHandle -> SigningMethod

-- | Represents a user's context for building and balancing transactions.
--   This is separate from the act of signing.
data Wallet = Wallet
  { wAddress      :: !GYAddress         -- ^ Primary address (often used for change).
  , wBalanceUtxos :: ![GYAddress]       -- ^ List of addresses to use for finding UTxOs for balancing.
  , wCollateral   :: !(Maybe GYTxOutRef)  -- ^ Optional collateral UTxO.
  }

-- | Loads a 'GYSigningKey' from a file path and constructs a 'Wallet'
--   (for balancing) and a 'SigningMethod' pair (for signing) from it.
--
--   This is a convenience helper for server-side or test environments.
loadWalletAndSigner
  :: FilePath           -- ^ Path to the user's signing key file (e.g., "keys/alice.skey").
  -> GYNetworkId
  -> IO (Wallet, (PubKeyHash, SigningMethod)) -- ^ Returns the wallet and the PKH/signer pair.
loadWalletAndSigner skeyPath networkId = do
  skey <- readSigningKey @'GYKeyRolePayment skeyPath
  let vkey = getVerificationKey skey
  let pkh = verificationKeyHash vkey
  let addr = addressFromPaymentKeyHash networkId pkh
  let plutusPkh = pubKeyHashToPlutus (toPubKeyHash pkh)

  -- Create a simple wallet context using the key's address
  let wallet = Wallet
        { wAddress = addr
        , wBalanceUtxos = [addr] -- Use primary address for balancing
        , wCollateral = Nothing -- TODO: Implement collateral finding
        }

  -- Create the signing method pair
  let signer = (plutusPkh, SignWithSKey skey)

  return (wallet, signer)