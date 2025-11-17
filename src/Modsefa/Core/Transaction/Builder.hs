{-|
Module      : Modsefa.Core.Transaction.Builder
Description : High-level transaction skeleton construction for Modsefa actions.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module provides the main entry point, 'buildTransactionDirect', for
constructing a complete transaction skeleton ('GeniusYield.TxBuilder.GYTxSkeleton')
from a Modsefa action specification ('Modsefa.Core.Foundation.Types.TypedActionSpec').

It orchestrates the process by:
1. Resolving derived parameters ('Modsefa.Core.Foundation.Types.ParamDerivation').
2. Determining the correct 'Modsefa.Core.Transaction.Types.RedeemerPolicy'.
3. Initializing the 'Modsefa.Core.Transaction.Context.TxBuilderContext' (e.g., with current time).
4. Processing the action's spending inputs ('Modsefa.Core.Foundation.Types.MustSpendActionParam', 'Modsefa.Core.Foundation.Types.MustSpendValidatorParam').
5. Processing the action's steps ('Modsefa.Core.Foundation.Types.ActionStep') via 'Modsefa.Core.Transaction.Operations.processActionSteps'.
6. Processing the action's constraints ('Modsefa.Core.Foundation.Types.TypedConstraint') via 'Modsefa.Core.Transaction.Constraints.processConstraintsDirectly'.
7. Combining all components into the final 'GeniusYield.TxBuilder.GYTxSkeleton'.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | High-level transaction building for the Modsefa library
--
-- This module provides the main entry points for building transactions
-- from action specifications. It orchestrates the operation processing,
-- constraint handling, and skeleton building.
--
-- Organization:
--   1. Main Transaction Building Entry Points
--   2. Helper Functions
--   3. Action Specification Processing
module Modsefa.Core.Transaction.Builder
  ( buildTransactionDirect
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.State (evalStateT, liftIO)
import Data.Map (empty, fromList)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text, pack)
import Data.Typeable (Typeable)
import GHC.TypeLits (KnownSymbol, symbolVal)

import GeniusYield.TxBuilder
  ( GYTxSkeleton(..), emptyGYTxSkeleton, mustHaveInput, runGYTxQueryMonadIO
  , slotOfCurrentBlock, slotToBeginTime
  )
import GeniusYield.Types 
  ( GYNetworkId, GYProviders, GYTxIn(GYTxIn), GYTxInWitness(GYTxInWitnessKey)
  , SingPlutusVersionI, timeToPlutus, txOutRefFromPlutus
  )
import PlutusLedgerApi.V1 (TxId(TxId), TxOutRef(..))
import PlutusLedgerApi.V3 (TxId(getTxId), TxOutRef(txOutRefId, txOutRefIdx))

import Modsefa.Core.Foundation
  ( ActionSpecName, ActionSpecParameters, ActionSpecSteps, AllStateTypesGeneric
  , AppSpec(AppInstanceParameters, ParameterDerivations, Validators)
  , DerivationContext, ExtractConstraintsFromAction, ExtractOpsFromAction
  , ExtractOpsFromActionSteps, ExtractPlutusVersion
  , ExtractPlutusVersionFromValidators, ExtractStateTypes, ParamsToValue
  , ResolveInstanceParamList, SomeFieldValue, TypedActionSpec
  )
import Modsefa.Core.Singletons
  ( AutoSingletonActionSpec(..), AutoSingletonParamDerivationList(..)
  , SActionSpec(..), SActionStep(SLet, SMap, SOp), SActionStepList(..)
  , SAppInstance(SAppInstance, instanceParams), SAppSpec
  , SConstraint(SMustSpendActionParam, SMustSpendValidatorParam)
  , SConstraintList(..), SInstanceParams, SOperationList(..), SParamDerivation(..)
  , SParamDerivationList(..), SParamTuple, extractParamByNameDirectWithProxy
  , extractParamNames
  )
import Modsefa.Core.ValidatorScript (AppValidatorScripts)

import Modsefa.Core.Transaction.Analysis (determineRedeemerPolicy)
import Modsefa.Core.Transaction.Constraints 
  ( ActionConstraintsValid, processConstraintsDirectly
  )
import Modsefa.Core.Transaction.Context
  ( TxBuilder(runTxBuilder), TxBuilderContext(..)
  )
import Modsefa.Core.Transaction.Operations (ProcessActionSteps, processActionSteps)
import Modsefa.Core.Transaction.Types (RedeemerPolicy)
import Modsefa.Core.Transaction.Utils
  ( extractSingleParamFromInstance, resolveDerivationSourceValue
  )


-- ============================================================================
-- 1. MAIN TRANSACTION BUILDING ENTRY POINTS
-- ============================================================================

-- | Builds a transaction skeleton ('GYTxSkeleton') directly from a 'TypedActionSpec'.
-- This is the primary function for converting a high-level Modsefa action definition
-- into a structure ready for balancing and signing with Genius Yield.
--
-- It requires numerous constraints to ensure the action specification and application
-- context are well-defined and possess the necessary capabilities (e.g., 'AppValidatorScripts',
-- 'ProcessActionSteps', 'ActionConstraintsValid', singleton auto-derivation).
buildTransactionDirect ::
  forall app (action :: TypedActionSpec).
  ( AutoSingletonActionSpec action
  , KnownSymbol (ActionSpecName action)
  , AllStateTypesGeneric (ExtractStateTypes (ExtractOpsFromAction action))
  , AppValidatorScripts app
  , Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
  , ActionConstraintsValid (ExtractConstraintsFromAction action)
  , SingPlutusVersionI (ExtractPlutusVersionFromValidators (Validators app))
  , ProcessActionSteps app action (ActionSpecSteps action) (ActionSpecParameters action) (ExtractPlutusVersion app)
  , AutoSingletonParamDerivationList (ParameterDerivations app)
  ) =>
  SAppInstance app -> -- ^ The specific application instance context (spec + instance params).
  Proxy action -> -- ^ A proxy indicating which action specification to build.
  SParamTuple (ActionSpecParameters action) -> -- ^ Value-level tuple containing the user-provided action parameters.
  GYNetworkId -> -- ^ The target Cardano network ID.
  GYProviders -> -- ^ Providers for blockchain interaction (querying time, UTxOs, etc.).
  IO (Either Text (GYTxSkeleton (ExtractPlutusVersion app))) -- ^ The resulting skeleton or an error message.
buildTransactionDirect appInstance@(SAppInstance appSpec' _) _actionProxy params networkId providers = do
  -- Resolve all derived parameters defined in the AppSpec.
  derivationContextResult <- resolveAllDerivations appInstance networkId providers
  case derivationContextResult of
    Left err -> return $ Left $ "Failed to resolve derived parameters: " <> err
    Right derivationContext -> do
      -- Query current on-chain time for transaction validity range.
      eTimeInfo <- try $ runGYTxQueryMonadIO networkId providers $ do
        slot <- slotOfCurrentBlock
        time <- slotToBeginTime slot
        return (slot, timeToPlutus time)

      case eTimeInfo of
        Left (e :: SomeException) -> return $ Left (pack $ "Failed to query on-chain time info: " ++ show e)
        Right (currentSlot, nowPlutus) -> do
          -- Automatically derive the action spec singleton.
          let actionSpec = autoSingletonActionSpec :: SActionSpec action
          -- Determine the redeemer strategy (e.g., use action name).
          let redeemerPolicy = determineRedeemerPolicy appSpec' actionSpec
          -- Initialize the transaction building context.
          let initialContext = TxBuilderContext
                { tbcCurrentTime = nowPlutus
                , tbcLetResults = empty
                , tbcResolvedRefs = empty
                , tbcResolvedFields = empty
                }
          -- Define the core transaction building logic within the TxBuilder monad.
          let txBuilderResult = processActionSpec appSpec' actionSpec params (instanceParams appInstance) redeemerPolicy derivationContext networkId providers

          -- Execute the TxBuilder computation.
          eSkel <- evalStateT (runTxBuilder txBuilderResult) initialContext

          case eSkel of
            Left err -> return $ Left err
            Right skeleton -> do
              -- Set the validity start slot based on queried time.
              let finalSkeleton = skeleton { gytxInvalidBefore = Just currentSlot }
              return $ Right finalSkeleton

-- ============================================================================
-- 2. HELPER FUNCTIONS
-- ============================================================================

-- | (Internal) Resolves the value for a single parameter derivation rule.
resolveSingleDerivation ::
  (AppSpec app, AppValidatorScripts app) =>
  SAppInstance app ->
  GYNetworkId ->
  GYProviders ->
  SParamDerivation derivation -> -- ^ The specific derivation rule singleton.
  IO (Either Text (Text, SomeFieldValue)) -- ^ Parameter name and its resolved value or error.
resolveSingleDerivation appInstance networkId providers (SDeriveParam paramProxy source) = do
  let paramName = pack $ symbolVal paramProxy
  -- Delegate to the utility function to resolve the source (address or hash).
  resolvedValueResult <- resolveDerivationSourceValue source appInstance networkId providers
  case resolvedValueResult of
    Left err -> return $ Left err
    Right resolvedValue -> return $ Right (paramName, resolvedValue)

-- | (Internal) Resolves all parameter derivation rules specified in the 'AppSpec'.
-- Collects the results into a 'DerivationContext' map.
resolveAllDerivations ::
  forall app.
  ( AppSpec app
  , AppValidatorScripts app
  , AutoSingletonParamDerivationList (ParameterDerivations app)
  ) =>
  SAppInstance app ->
  GYNetworkId ->
  GYProviders ->
  IO (Either Text DerivationContext) -- ^ Map of derived parameter names to resolved values, or error.
resolveAllDerivations appInstance@(SAppInstance _appSpec _) networkId providers = do
  -- Get the singleton list of derivation rules.
  let derivations = autoSingletonParamDerivationList @(ParameterDerivations app)
  -- Resolve each rule recursively.
  results <- resolveDerivations' derivations
  -- Check if any resolution failed.
  case sequence results of
    Left err -> return $ Left err
    Right resolvedList -> return $ Right $ fromList resolvedList
  where
    -- Recursive helper to process the SParamDerivationList.
    resolveDerivations' :: SParamDerivationList ds -> IO [Either Text (Text, SomeFieldValue)]
    resolveDerivations' SPDLNil = return []
    resolveDerivations' (SPDLCons d rest) = do
      r <- resolveSingleDerivation appInstance networkId providers d -- Resolve head
      rs <- resolveDerivations' rest -- Resolve tail
      return (r : rs)

-- ============================================================================
-- 3. ACTION SPECIFICATION PROCESSING
-- ============================================================================

-- | (Internal) Orchestrates the main steps of building a transaction skeleton for a specific action.
-- Called by 'buildTransactionDirect'.
processActionSpec ::
  forall app (action :: TypedActionSpec) pv.
  ( AllStateTypesGeneric (ExtractStateTypes (ExtractOpsFromAction action))
  , AppValidatorScripts app
  , SingPlutusVersionI pv
  , Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
  , ProcessActionSteps app action (ActionSpecSteps action) (ActionSpecParameters action) pv
  , ActionConstraintsValid (ExtractConstraintsFromAction action)
  ) =>
  SAppSpec app -> -- ^ Application spec singleton.
  SActionSpec action -> -- ^ Action spec singleton.
  SParamTuple (ActionSpecParameters action) -> -- ^ Action parameters.
  SInstanceParams app -> -- ^ Instance parameters.
  RedeemerPolicy -> -- ^ Redeemer policy for this action.
  DerivationContext -> -- ^ Resolved derived parameters.
  GYNetworkId -> -- ^ Network ID.
  GYProviders -> -- ^ Blockchain providers.
  TxBuilder pv (Either Text (GYTxSkeleton pv)) -- ^ Resulting skeleton or error within the TxBuilder monad.
processActionSpec appSpec' actionSpec@(SActionSpec nameProxy steps constraints paramSpecs) params instanceParams' redeemerPolicy derivationContext networkId providers = do
  liftIO $ putStrLn $ "Processing action: " ++ symbolVal nameProxy

  -- Extract parameter names for indexed access later.
  let paramNames = extractParamNames paramSpecs
  let initialSkeleton = emptyGYTxSkeleton

  -- 1. Add required spending inputs (from MustSpend*Param constraints).
  -- This is done in IO as it might involve looking up UTxO details (though current impl might not).
  skelWithInputsResult <- liftIO $ addSpendingInputs constraints params paramNames instanceParams' initialSkeleton
  case skelWithInputsResult of
    Left err -> return $ Left err
    Right skelWithInputs -> do
      -- Pass the context down to the next level
      skeletonWithOutputsResult <- processActionSteps appSpec' actionSpec params paramNames instanceParams' redeemerPolicy derivationContext networkId providers skelWithInputs

      case skeletonWithOutputsResult of
        Left err -> return $ Left err
        Right skeleton -> do
          let operations = extractOpsFromSteps steps
          processConstraintsDirectly constraints operations appSpec' params paramNames instanceParams' redeemerPolicy networkId providers skeleton
  where
    -- Helper to extract the list of operations from the steps list.
    extractOpsFromSteps :: SActionStepList s -> SOperationList (ExtractOpsFromActionSteps s)
    extractOpsFromSteps ASSLNil = SOLNil
    extractOpsFromSteps (ASSLCons (SOp op) r) = SOLCons op (extractOpsFromSteps r)
    extractOpsFromSteps (ASSLCons (SLet _ op) r) = SOLCons op (extractOpsFromSteps r)
    extractOpsFromSteps (ASSLCons (SMap op _ _) r) = SOLCons op (extractOpsFromSteps r)


-- | (Internal) Adds required spending inputs to the skeleton based on 'MustSpendActionParam'
-- and 'MustSpendValidatorParam' constraints. Assumes these inputs are spent with key witnesses.
addSpendingInputs ::
  ( Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
  ) =>
  SConstraintList cs ->
  SParamTuple ps ->
  [Text] ->
  SInstanceParams app ->
  GYTxSkeleton pv ->
  IO (Either Text (GYTxSkeleton pv))
addSpendingInputs SCLNil _ _ _ skeleton = return $ Right skeleton
addSpendingInputs (SCLCons constraint rest) params paramNames instanceParams' skeleton = do
  -- Process the current constraint
  newSkelRes <- case constraint of
    -- Constraint requires spending a UTxO from validator instance parameters
    SMustSpendValidatorParam _vNameProxy paramProxy -> do
      let paramName = pack $ symbolVal paramProxy
      case extractSingleParamFromInstance @PlutusLedgerApi.V3.TxOutRef paramName instanceParams' of
        Left err -> return $ Left $ "Constraint requires spending instance parameter '" <> paramName <> "', but it could not be extracted: " <> err
        Right txOutRefV3 -> do
          let txOutRefV1 = PlutusLedgerApi.V1.TxOutRef (PlutusLedgerApi.V1.TxId (PlutusLedgerApi.V3.getTxId (PlutusLedgerApi.V3.txOutRefId txOutRefV3))) (PlutusLedgerApi.V3.txOutRefIdx txOutRefV3)

          case txOutRefFromPlutus txOutRefV1 of
            Left err' -> return $ Left $ "Failed to convert TxOutRef: " <> pack (show err')
            Right gyTxOutRef -> do
              -- Add the input with a key witness to the skeleton
              let input = mustHaveInput $ GYTxIn gyTxOutRef GYTxInWitnessKey
              return $ Right $ skeleton <> input

    -- Constraint requires spending a UTxO from action parameters
    SMustSpendActionParam paramProxy -> do
      let paramName = pack $ symbolVal paramProxy
      case extractParamByNameDirectWithProxy (Proxy @PlutusLedgerApi.V3.TxOutRef) paramName paramNames params of
        Left err -> return $ Left $ "Constraint requires spending action parameter '" <> paramName <> "', but it could not be extracted: " <> err
        Right txOutRefV3 -> do
          let txOutRefV1 = PlutusLedgerApi.V1.TxOutRef (PlutusLedgerApi.V1.TxId (getTxId (PlutusLedgerApi.V3.txOutRefId txOutRefV3))) (PlutusLedgerApi.V3.txOutRefIdx txOutRefV3)
          case txOutRefFromPlutus txOutRefV1 of
            Left err' -> return $ Left $ "Failed to convert TxOutRef from action parameter: " <> pack (show err')
            Right gyTxOutRef -> do
              -- Add the input with a key witness to the skeleton
              let input = mustHaveInput $ GYTxIn gyTxOutRef GYTxInWitnessKey
              return $ Right $ skeleton <> input
    -- Other constraints don't add spending inputs here
    _ -> return $ Right skeleton

  -- Recurse or return error
  case newSkelRes of
    Left err -> return $ Left err
    Right newSkel -> addSpendingInputs rest params paramNames instanceParams' newSkel