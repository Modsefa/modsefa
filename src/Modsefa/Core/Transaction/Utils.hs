{-|
Module      : Modsefa.Core.Transaction.Utils
Description : Utility functions for Modsefa transaction building and client support.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module provides various utility functions supporting the transaction
building subsystem and client interactions. Key functionalities include:

- Resolving on-chain script addresses ('GYAddress') and script hashes ('ScriptHash') for validators within a specific application instance ('SAppInstance').
- Extracting and resolving validator parameters ('ParamsToValue') considering instance parameters ('SInstanceParams') and derivation rules ('ParamDerivation', 'SDerivedParams').
- Handling parameter derivations ('SDerivationSource') by querying necessary on-chain information.
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Utility functions for transaction building and client support
--
-- This module provides utility functions needed for client operations,
-- validator address resolution, and other supporting functionality for
-- the transaction building system.
--
-- Organization:
--   1. Validator Address Resolution
--   2. Validator Parameter Extraction
--   3. Parameter Derivation Support
--   4. Supporting Types
module Modsefa.Core.Transaction.Utils
  ( -- * Validator Address Resolution
    getValidatorAddressFromInstance
  , findValidatorWithScript

    -- * Validator Parameter Extraction
  , extractValidatorParamsFromAppInstance
  , extractSingleParamFromInstance

    -- * Parameter Derivation Support
  , resolveDerivationSourceValue
  , resolveValidatorParameters
  , findParameterDerivationFor
  , findInDerivationList
  , resolveValidatorParametersFromAllSources

    -- * Supporting Types
  , SomeValidatorWithScript(..)
  , SomeParameterDerivation(..)
  ) where

import Data.Proxy (Proxy(Proxy))
import Data.Text (Text, pack)
import Data.Typeable (Typeable, cast, typeOf, typeRep)
import GHC.TypeLits (symbolVal)

import GeniusYield.TxBuilder (runGYTxQueryMonadIO, scriptAddress)
import GeniusYield.Types
  ( GYAddress, GYNetworkId, GYProviders, addressToPlutus, scriptPlutusHash
  )
import PlutusLedgerApi.V3 (Address, ScriptHash)

import Modsefa.Core.Foundation
  ( AppSpec(Validators), ParamsToValue, SomeFieldValue(..), ValidatorSpec(Params)
  )
import Modsefa.Core.Singletons
  ( SAppInstance(SAppInstance), SAppSpec(..), SDerivationSource(..)
  , SInstanceParams(..), SParamDerivation(..), SParamDerivationList(..)
  , SParamList(SPCons, SPNil), SParamTuple(STupleNil), SValidator(..)
  , SValidatorList(..), SomeValidator(SomeValidator), findValidatorByName
  , getValidatorNameFromSingleton
  )
import Modsefa.Core.ValidatorScript (AppValidatorScripts(..))

import Modsefa.Core.Transaction.Parameters (extractFromDerivedParams)
import Modsefa.Core.Transaction.Types (SDerivedParams(..), SomeDerivedParams(..))


-- ============================================================================
-- 1. VALIDATOR ADDRESS RESOLUTION
-- ============================================================================

-- | Resolves the on-chain script address ('GYAddress') for a validator within a specific application instance.
-- It finds the validator by name, resolves its parameters (considering derivations),
-- obtains the 'GYScript', and calculates the address for the given network.
getValidatorAddressFromInstance ::
  forall app.
  ( AppSpec app, AppValidatorScripts app ) =>
  Text ->                -- ^ Validator name ('ValidatorAppName').
  SAppInstance app ->    -- ^ The application instance context.
  GYNetworkId ->         -- ^ The target network ID.
  GYProviders ->         -- ^ Blockchain query providers.
  IO (Either Text GYAddress) -- ^ 'Right GYAddress' on success, 'Left error' on failure.
getValidatorAddressFromInstance validatorName appInstance@(SAppInstance appSpec' instanceParams') networkId providers = do
  -- Find the validator's singleton definition and script capability.
  case findValidatorWithScript validatorName appSpec' of
    Nothing -> return $ Left $ "Validator '" <> validatorName <> "' not found or doesn't provide a script."
    Just (SomeValidatorWithScript proxy validator) -> do
      -- Check if this validator's parameters are derived.
      case findParameterDerivationFor validatorName appSpec' of
        -- Case 1: Parameters are NOT derived. Resolve directly from instance/action params (currently assumes only instance params needed here).
        Nothing -> do
          case extractValidatorParamsFromAppInstance validator instanceParams' of
            Left err -> return $ Left err
            Right validatorParams -> do
              let script = getValidatorScript @app proxy validatorParams
              address <- runGYTxQueryMonadIO networkId providers $ scriptAddress script
              return $ Right address

        -- Case 2: Parameters ARE derived.
        Just derivation -> do
          -- Resolve the derived parameter values first.
          derivedParamsResult <- resolveValidatorParameters validatorName derivation appInstance networkId providers
          case derivedParamsResult of
            Left err -> return $ Left $ "Failed to resolve derived parameters for '" <> validatorName <> "': " <> err
            Right derivedParams -> do
              -- Now resolve the full parameter set using the derived values.
              case resolveValidatorParametersFromAllSources validator appInstance (Just derivedParams) STupleNil [] of
                Left err -> return $ Left err
                Right validatorParams -> do
                  let script = getValidatorScript @app proxy validatorParams
                  address <- runGYTxQueryMonadIO networkId providers $ scriptAddress script
                  return $ Right address

-- | Resolves the Plutus script hash ('ScriptHash') for a validator within a specific application instance.
-- Similar logic to 'getValidatorAddressFromInstance' but calculates the hash instead of the address.
getValidatorHashFromInstance ::
  forall app.
  ( AppSpec app, AppValidatorScripts app ) =>
  Text ->                -- ^ Validator name ('ValidatorAppName').
  SAppInstance app ->    -- ^ The application instance context.
  GYNetworkId ->         -- ^ The target network ID (used for parameter resolution).
  GYProviders ->         -- ^ Blockchain query providers (used for parameter resolution).
  IO (Either Text ScriptHash) -- ^ 'Right ScriptHash' on success, 'Left error' on failure.
getValidatorHashFromInstance validatorName _appInstance@(SAppInstance appSpec' instanceParams') _networkId _providers = do
  case findValidatorWithScript validatorName appSpec' of
    Nothing -> return $ Left $ "Validator '" <> validatorName <> "' not found or doesn't provide a script."
    Just (SomeValidatorWithScript proxy validator) -> do
      case extractValidatorParamsFromAppInstance validator instanceParams' of
        Left err -> return $ Left err
        Right validatorParams -> do
          let script = getValidatorScript @app proxy validatorParams
          let scriptHash = scriptPlutusHash script
          return $ Right scriptHash

-- | Finds a validator singleton ('SomeValidatorWithScript') by name within an 'SAppSpec',
-- ensuring it has script capabilities via 'AppValidatorScripts'.
findValidatorWithScript ::
  forall app. AppValidatorScripts app =>
  Text -> -- ^ Target validator name.
  SAppSpec app -> -- ^ Application specification singleton.
  Maybe (SomeValidatorWithScript app) -- ^ 'Just' wrapper if found, 'Nothing' otherwise.
findValidatorWithScript targetName appSpec' =
  findInValidatorListWithScript targetName (getValidatorListFromSpec appSpec')

-- | (Internal) Helper for 'findValidatorWithScript': Recursively searches 'SValidatorList'.
findInValidatorListWithScript ::
  forall app validators. AppValidatorScripts app =>
  Text ->
  SValidatorList validators ->
  Maybe (SomeValidatorWithScript app)
findInValidatorListWithScript _ SVNil = Nothing
findInValidatorListWithScript targetName (SVCons validator rest) =
  case checkValidatorMatch targetName validator of
    Just someValidatorWithScript -> Just someValidatorWithScript
    Nothing -> findInValidatorListWithScript targetName rest

-- | (Internal) Checks if an 'SValidator' matches the target name and wraps it.
checkValidatorMatch ::
  forall app v. ( ValidatorSpec v, Typeable v, AppValidatorScripts app ) =>
  Text ->
  SValidator v ->
  Maybe (SomeValidatorWithScript app)
checkValidatorMatch targetName validator =
  let validatorName = getValidatorNameFromSingleton validator
  in if validatorName == targetName
     then Just (SomeValidatorWithScript (Proxy @v) validator)
     else Nothing

-- | (Internal) Extracts the 'SValidatorList' from an 'SAppSpec'.
getValidatorListFromSpec :: SAppSpec app -> SValidatorList (Validators app)
getValidatorListFromSpec (SAppSpec validators _ _ _ _ _) = validators

-- ============================================================================
-- 2. VALIDATOR PARAMETER EXTRACTION
-- ============================================================================

-- | Extracts validator parameters ('ParamsToValue') solely from the application instance parameters ('SInstanceParams').
-- WARNING: This function does NOT consider derived parameters or action parameters.
-- It's likely only suitable for simple cases or potentially initialization.
-- Consider using 'resolveValidatorParametersFromAllSources' for general use.
extractValidatorParamsFromAppInstance ::
  forall v app. ValidatorSpec v =>
  SValidator v ->
  SInstanceParams app ->
  Either Text (ParamsToValue (Params v))
extractValidatorParamsFromAppInstance validator instanceParams' =
  case validator of
    -- No params case
    SValidator SPNil _ _ _ -> Right ()
    -- Single param case: Delegate to extractSingleParamFromInstance
    SValidator (SPCons nameProxy _typeProxy SPNil) _ _ _ -> do
      let paramName = pack $ symbolVal nameProxy
      -- Assuming the type 't' from '(name, t)' corresponds to ParamsToValue '[ '(name, t) ]
      extractSingleParamFromInstance paramName instanceParams'
    -- Multi-param case: Not supported by this simplified function
    _ -> Left $ "extractValidatorParamsFromAppInstance: Multi-parameter validators not supported. Validator: " <> getValidatorNameFromSingleton validator

-- | Extracts a single parameter value of type 'a' from 'SInstanceParams'.
-- WARNING: Only works if the 'SInstanceParams' holds exactly one value of type 'a'.
-- Does NOT handle multiple instance parameters or look up by name. Very limited utility.
-- Consider using 'resolveValidatorParametersFromAllSources'.
extractSingleParamFromInstance ::
  forall a app. Typeable a =>
  Text -> -- ^ Parameter name (only used for error messages).
  SInstanceParams app -> -- ^ Instance parameters wrapper.
  Either Text a -- ^ Resulting value or error.
extractSingleParamFromInstance paramName (SInstanceParams instanceParamValues) =
  case cast instanceParamValues of -- Attempt to cast the *entire* instanceParamValues
    Just (value :: a) -> Right value  -- Cast successful
    Nothing -> Left $ "Instance parameter type mismatch for: " <> paramName <>
                     ". Expected: " <> pack (show (typeRep (Proxy :: Proxy a))) <>
                     ", but SInstanceParams contained: " <> pack (show (typeOf instanceParamValues))

-- ============================================================================
-- 3. PARAMETER DERIVATION SUPPORT
-- ============================================================================

-- | Resolves the value ('SomeFieldValue') specified by a 'DerivationSource' singleton.
-- Performs necessary IO actions (like getting addresses/hashes) based on the source type.
resolveDerivationSourceValue ::
  ( AppSpec app
  , AppValidatorScripts app
  ) =>
  SDerivationSource source -> -- ^ The singleton representing the derivation source.
  SAppInstance app -> -- ^ The application instance context.
  GYNetworkId -> -- ^ Network ID.
  GYProviders -> -- ^ Blockchain providers.
  IO (Either Text SomeFieldValue) -- ^ 'Right SomeFieldValue' containing the resolved value, or 'Left error'.
resolveDerivationSourceValue source appInstance networkId providers = do
  case source of
    -- Source is another validator's address
    SValidatorAddress validatorProxy -> do
      let sourceValidatorName = pack $ symbolVal validatorProxy
      -- Get the address using the function defined above
      addrResult <- getValidatorAddressFromInstance sourceValidatorName appInstance networkId providers
      case addrResult of
        Left err -> return $ Left $ "Failed to get address for derivation source '" <> sourceValidatorName <> "': " <> err
        Right addr -> return $ Right $ SomeFieldValue $ addressToPlutus addr
    SValidatorHash validatorProxy -> do
        let sourceValidatorName = pack $ symbolVal validatorProxy
        -- Get the hash using the function defined above
        hashResult <- getValidatorHashFromInstance sourceValidatorName appInstance networkId providers
        case hashResult of
          Left err -> return $ Left $ "Failed to get script hash for derivation source '" <> sourceValidatorName <> "': " <> err
          Right hash -> return $ Right $ SomeFieldValue hash

-- | Finds the 'ParamDerivation' rule ('SomeParameterDerivation') for the first parameter
-- of a given validator, if such a rule exists in the 'SAppSpec'.
-- Note: Assumes the relevant derivation rule corresponds to the validator's first parameter.
findParameterDerivationFor :: forall app. Text -> SAppSpec app -> Maybe SomeParameterDerivation
findParameterDerivationFor validatorName appSpec' =
  case appSpec' of
    SAppSpec validatorList _ _ _ _ derivationList ->
      -- Find the validator by name
      case findValidatorByName validatorList validatorName of
        Nothing -> Nothing -- Validator not found
        Just (SomeValidator (SValidator paramList _ _ _)) ->
          -- Get the name of the first parameter of that validator
          case paramList of
            SPNil -> Nothing -- Validator has no parameters
            SPCons nameProxy _ _ -> -- Found the first parameter name
              let paramNameToFind = pack (symbolVal nameProxy)
              -- Search the app's derivation list for a rule matching this parameter name
              in findInDerivationList paramNameToFind derivationList

-- | Searches an 'SParamDerivationList' for a rule matching the target parameter name.
findInDerivationList :: Text -> SParamDerivationList derivations -> Maybe SomeParameterDerivation
findInDerivationList _ SPDLNil = Nothing
findInDerivationList targetParamName (SPDLCons derivation rest) =
  case derivation of
    SDeriveParam paramNameProxy _ ->
      let paramName = pack (symbolVal paramNameProxy)
      in if paramName == targetParamName
         then Just (SomeParameterDerivation derivation)
         else findInDerivationList targetParamName rest

-- | Resolves the value specified by a 'SomeParameterDerivation' rule by calling 'resolveDerivationSourceValue'.
-- Returns the resolved value wrapped in 'SomeDerivedParams' (which contains the parameter name and value).
resolveValidatorParameters ::
  forall app.
  ( AppSpec app
  , AppValidatorScripts app
  ) =>
  Text -> -- ^ Validator name (used for error context, maybe?).
  SomeParameterDerivation -> -- ^ The derivation rule to resolve.
  SAppInstance app -> -- ^ Application instance context.
  GYNetworkId -> -- ^ Network ID.
  GYProviders -> -- ^ Blockchain providers.
  IO (Either Text SomeDerivedParams) -- ^ 'Right SomeDerivedParams' or 'Left error'.
resolveValidatorParameters _validatorName (SomeParameterDerivation derivation) appInstance networkId providers = do
  case derivation of
    -- Extract the parameter name proxy and the derivation source singleton
    SDeriveParam paramProxy source -> do
      -- Resolve the source value (e.g., get the address/hash)
      resolvedValueResult <- resolveDerivationSourceValue source appInstance networkId providers
      case resolvedValueResult of
        Left err -> return $ Left err
        Right (SomeFieldValue resolvedValue) ->
          -- Construct the SDerivedParams singleton containing the name and resolved value.
          -- This relies on the Typeable constraint carried by SomeFieldValue and SDerivedParams.
          -- It assumes the type 't' in ParamDerivation matches the resolved value's type.
          -- Inner case block handles different source types to ensure type safety if needed (though SomeFieldValue abstracts it).
          case source of
            SValidatorAddress _ -> case cast resolvedValue of
              Just (addr :: Address) -> return $ Right $ SomeDerivedParams (SDPCons paramProxy addr SDPNil)
              Nothing -> return $ Left "Internal Error: Derived value for ValidatorAddress was not an Address."
            SValidatorHash _ -> case cast resolvedValue of
              Just (hash :: ScriptHash) -> return $ Right $ SomeDerivedParams (SDPCons paramProxy hash SDPNil)
              Nothing -> return $ Left "Internal Error: Derived value for ValidatorHash was not a ScriptHash."

-- | Resolves the complete parameter value ('ParamsToValue') for a validator, considering all sources.
-- It prioritizes derived parameters if available ('Just derivedParams'), otherwise falls back
-- to action parameters ('actionParams') and finally instance parameters ('appInstance').
resolveValidatorParametersFromAllSources ::
  forall v app params. (ValidatorSpec v, AppSpec app) =>
  SValidator v -> -- ^ The singleton for the validator whose parameters are needed.
  SAppInstance app -> -- ^ The application instance context.
  Maybe SomeDerivedParams -> -- ^ 'Just' resolved derived parameters, or 'Nothing'.
  SParamTuple params -> -- ^ Action parameters provided by the user.
  [Text] -> -- ^ List of action parameter names.
  Either Text (ParamsToValue (Params v)) -- ^ 'Right' the final resolved parameter value, or 'Left error'.
resolveValidatorParametersFromAllSources validator appInstance mDerivedParams actionParams actionParamNames =
  case validator of
    -- Base case: Validator requires no parameters.
    SValidator SPNil _ _ _ -> Right ()
    SValidator (SPCons nameProxy _typeProxy SPNil) _ _ _ -> do
      let paramName = pack $ symbolVal nameProxy
      -- Delegate to the corrected single-parameter resolution function.
      resolveSingleParameterTypeSafe paramName appInstance mDerivedParams actionParams actionParamNames
    _ -> Left "Multi-parameter validators not yet supported in derivation resolution"

-- | (Internal) Resolves a *single* parameter value of type 'a', prioritizing derived parameters.
-- If not found in derived parameters (or if 'mDerivedParams' is 'Nothing'), falls back to
-- 'extractParamFromBothSources' (which checks action params then instance params).
resolveSingleParameterTypeSafe ::
  forall a app params.
  (Typeable a, AppSpec app) =>
  Text ->
  SAppInstance app ->
  Maybe SomeDerivedParams ->
  SParamTuple params ->
  [Text] ->
  Either Text a
resolveSingleParameterTypeSafe paramName appInstance mDerivedParams _actionParams _actionParamNames =
  case mDerivedParams of
    -- If derived parameters are provided, only look there.
    Just (SomeDerivedParams derivedParams) ->
      extractFromDerivedParams paramName derivedParams

    -- If no derived parameters, use the standard fallback logic.
    Nothing ->
      let (SAppInstance _ instanceParams') = appInstance
      in extractSingleParamFromInstance paramName instanceParams'

-- ============================================================================
-- 4. SUPPORTING TYPES
-- ============================================================================

-- | Existential wrapper combining an 'SValidator' with its 'Proxy'.
-- Includes constraints ensuring the validator has script capabilities ('AppValidatorScripts').
-- Used by functions needing both the singleton and the type proxy for script retrieval.
data SomeValidatorWithScript app where
  SomeValidatorWithScript ::
    ( ValidatorSpec v, Typeable v, AppValidatorScripts app ) =>
    Proxy v -> SValidator v -> SomeValidatorWithScript app

-- | Existential wrapper for 'SParamDerivation', hiding the specific derivation type.
data SomeParameterDerivation where
  SomeParameterDerivation :: SParamDerivation derivation -> SomeParameterDerivation