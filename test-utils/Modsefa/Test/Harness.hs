{-|
Module      : Modsefa.Test.Harness
Description : A simple test harness for running Modsefa client scenarios.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module provides a reusable test harness function, 'runScenario',
to reduce boilerplate in the example test suites. It handles client loading,
action execution, result printing, and verification.
-}
{-# LANGUAGE ScopedTypeVariables #-}

module Modsefa.Test.Harness
  ( runScenario
  , handleResult
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Data.Text (Text, pack, unpack)

import GeniusYield.Types (GYTxId)

import Modsefa.Client (ClientEnv, ModsefaClient(..), loadModsefaClient)
import Modsefa.Core.Singletons (SAppInstance)


-- | A generic function to run a complete Modsefa test scenario.
-- This handles client loading, action execution, result printing,
-- and verification.
runScenario
  :: forall app.
     String                                  -- ^ Test name for logging.
  -> FilePath                                -- ^ Path to the config.json file.
  -> FilePath                                -- ^ Path to the signer's skey file.
  -> SAppInstance app                        -- ^ The application instance to test.
  -> (ModsefaClient -> IO (Either Text GYTxId)) -- ^ The client action to run.
  -> (ClientEnv -> SAppInstance app -> IO Bool) -- ^ The verification function.
  -> IO ()
runScenario testName configPath skeyPath appInstance runAction verify = do
  putStrLn $ "--- Running Scenario: " ++ testName ++ " ---"

  -- 1. Load client using the provided paths
  eClient <- try $ loadModsefaClient configPath skeyPath (pack testName)

  case eClient of
    Left (e :: SomeException) -> do
      putStrLn "❌ ERROR: Failed to load ModsefaClient."
      putStrLn $ "   Please ensure '" ++ configPath ++ "' and '" ++ skeyPath ++ "' exist."
      putStrLn "   You can generate keys by running: ./scripts/keygen.sh <key-name>"
      putStrLn $ "   Error details: " ++ show e
    Right client -> do
      -- 2. Run the action
      result <- runAction client
      handleResult testName result

      -- 3. Run verification on success
      case result of
        Left _ -> putStrLn "Skipping verification due to action failure."
        Right _ -> do
          putStrLn "\n-- Verifying State --"
          putStrLn "(Waiting 5 seconds for chain index...)"
          threadDelay 5000000 -- 5 seconds
          
          success <- verify (mcEnv client) appInstance
          
          if success
            then putStrLn "✅ Verification: Success."
            else putStrLn "❌ Verification: Failed."

  putStrLn $ "--- Scenario " ++ testName ++ " Complete ---"


-- | Helper to print action results consistently.
handleResult :: String -> Either Text GYTxId -> IO ()
handleResult actionName result =
  case result of
    Left err   -> putStrLn $ "❌ " ++ actionName ++ " failed: " ++ unpack err
    Right txId -> putStrLn $ "✅ " ++ actionName ++ " succeeded! TxId: " ++ show txId