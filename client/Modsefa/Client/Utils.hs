{-|
Module      : Modsefa.Client.Utils
Description : General utility functions for Modsefa client applications.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Modsefa.Client.Utils
  ( getPkhFromSKeyFile
  , queryAndPrintState
  , gyTxOutRefToV3
  ) where

import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Text.Pretty.Simple (pPrint)

import GeniusYield.Types
  ( AsPubKeyHash(toPubKeyHash), GYKeyRole(..), GYTxOutRef, getVerificationKey
  , pubKeyHashToPlutus, readSigningKey, txOutRefToPlutus, verificationKeyHash
  )
import PlutusLedgerApi.V1 (getTxId, txOutRefId, txOutRefIdx)
import PlutusLedgerApi.V3 (PubKeyHash, TxId(TxId), TxOutRef(TxOutRef))

import Modsefa.Core.Foundation (GetStateData, GetStateName)
import Modsefa.Core.Singletons (SAppInstance)

import Modsefa.Client.Types (StateInstance(siData))


-- | Reads a payment signing key from a file and derives the corresponding Plutus PubKeyHash.
--   Requires network configuration to determine the correct address derivation.
getPkhFromSKeyFile
  :: FilePath -- ^ Path to the user's payment signing key file (e.g., "spender.skey").
  -> IO PubKeyHash
getPkhFromSKeyFile skeyPath = do
  -- Read the signing key
  skey <- readSigningKey @'GYKeyRolePayment skeyPath
  -- Derive Plutus PubKeyHash
  let pkh = pubKeyHashToPlutus $ toPubKeyHash $ verificationKeyHash $ getVerificationKey skey
  return pkh

-- | Executes a Modsefa state query function for a given app instance
--   and pretty-prints the result(s).
queryAndPrintState
  :: forall app st f.
     (Show (GetStateData st), KnownSymbol (GetStateName st)) -- Require Show and KnownSymbol
  => (SAppInstance app -> IO (f (StateInstance st))) -- Query function (f is Maybe or [])
  -> (f (StateInstance st) -> [StateInstance st]) -- Function to extract list
  -> SAppInstance app -- The application instance to query
  -> IO ()
queryAndPrintState queryFn extractFn appInstance = do
  let stateName = symbolVal (Proxy @(GetStateName st)) -- Get state name from type
  putStrLn $ "Querying " ++ stateName ++ " state..." -- Use state name in log
  result <- queryFn appInstance
  let instances = extractFn result
  if null instances
    then putStrLn $ "  No " ++ stateName ++ " instances found."
    else do
      putStrLn $ "  Found " ++ show (length instances) ++ " instance(s):"
      -- pPrint prints the whole StateInstance; use siData to print only the datum
      mapM_ (pPrint . siData) instances

-- | Converts a Genius Yield TxOutRef to a Plutus V3 TxOutRef.
gyTxOutRefToV3 :: GYTxOutRef -> TxOutRef
gyTxOutRefToV3 gyRef =
  let pv1Ref = txOutRefToPlutus gyRef
  in TxOutRef (TxId (getTxId (txOutRefId pv1Ref))) (txOutRefIdx pv1Ref)