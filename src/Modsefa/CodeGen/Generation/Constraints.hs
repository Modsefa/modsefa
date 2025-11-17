{-|
Module      : Modsefa.CodeGen.Generation.Constraints
Description : Plutus Tx code generation for Modsefa constraints from IR.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC (requires TemplateHaskell)

This module uses Template Haskell (TH) to generate Plutus Tx code fragments
('Language.Haskell.TH.Exp') that implement the on-chain validation logic
for various constraints defined in the Modsefa Intermediate Representation
('Modsefa.Core.IR.Types.ConstraintIR').

The main function, 'generateConstraintChecksForAction', takes an 'ActionIR' and
produces a single TH expression that combines the checks for all constraints
within that action using boolean AND ('PlutusTx.Prelude.&&'). Individual helper
functions generate the code for specific constraint types like 'MustBeSignedBy',
'MustAddToAggregateState', 'MustWithdrawFromAggregateState', and 'MustCheckInstance'.
-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

-- | Constraint validation code generation from the IR
--
-- This module generates Template Haskell code for validating constraints
-- within validator functions, using the backend-agnostic IR.
module Modsefa.CodeGen.Generation.Constraints
  ( generateConstraintChecksForAction
  ) where

import Control.Monad (MonadFail(fail))
import Data.Text (Text, unpack)
import Language.Haskell.TH
  ( Exp, Q, Type(..), conT, listE, litE, mkName, stringL, varE
  )
import Prelude (String, return, (++), ($), (==))

import PlutusLedgerApi.V1.Value (AssetClass(AssetClass), assetClass, valueOf)
import PlutusLedgerApi.V3
  ( Address(..), Credential(ScriptCredential), CurrencySymbol(CurrencySymbol)
  , Datum(..), OutputDatum(..), PubKeyHash, ScriptHash(ScriptHash), TokenName(..)
  , TxInInfo, TxOut, Value(..), scriptContextTxInfo, txInInfoResolved, txInfoInputs
  , txInfoOutputs, txInfoReferenceInputs, txInfoSignatories, txOutAddress
  , txOutDatum, txOutValue
  )
import PlutusTx (fromBuiltinData)
import PlutusTx.Builtins.HasOpaque
  ( stringToBuiltinByteString, stringToBuiltinString
  )
import PlutusTx.Prelude
  ( Bool(..), Maybe(..), all, any, appendString, elem, emptyString
  , find, id, map, not, traceError, traceIfFalse, (++), (==), (&&), (>)
  , (>=)
  )

import Modsefa.Core.IR.Types
  ( ActionIR(..), ConstraintIR(..), FieldValueIR(FromActionParam)
  , InstanceCheckIR(..), PolicySourceIR(..), PubKeyHashIR(..), StateInfoIR(..)
  )

import Modsefa.CodeGen.Generation.InstanceChecks (generateAddressMatchesParamCheck)
import Modsefa.CodeGen.Generation.ValueResolution
  ( generateAddressExpression, generatePlutusValueExpression
  )


-- ============================================================================
-- 1. MAIN ENTRY POINT
-- ============================================================================

-- | Generates a Plutus Tx boolean expression ('Q Exp') that performs all necessary
-- on-chain validation checks for the constraints specified within a given 'ActionIR'.
generateConstraintChecksForAction :: [StateInfoIR] -- ^ The application state registry.
                                  -> ActionIR -- ^ The action IR.
                                  -> Q Exp -- ^ Combined constraint check expression.
generateConstraintChecksForAction registry action = do
  let constraintChecks = map (generateSingleConstraintCheck registry action) (actionIRConstraints action)
  [| PlutusTx.Prelude.all PlutusTx.Prelude.id $(listE constraintChecks) |]

-- ============================================================================
-- 2. INDIVIDUAL CONSTRAINT GENERATION
-- ============================================================================

-- | (Internal) Generates the validation logic for a single 'ConstraintIR'.
-- Dispatches to specific helper functions based on the constraint constructor.
generateSingleConstraintCheck :: [StateInfoIR] -- ^ Mapping of state type to datum type
                              -> ActionIR -- ^ The parent action IR (needed for context, e.g., resolving 'FieldValueIR').
                              -> ConstraintIR -- ^ The specific constraint IR to generate code for.
                              -> Q Exp -- ^ TH computation returning the validation expression for this constraint.
-- Dispatch based on constraint type:
generateSingleConstraintCheck registry _action (MustBeSignedBy pkhSource) =
  generateMustBeSignedByCheck registry pkhSource
-- MustSpendActionParam is checked off-chain during transaction building, not in the validator.
generateSingleConstraintCheck _registry _action (MustSpendActionParamIR _) =
  [| True |]
generateSingleConstraintCheck registry action (MustAddToAggregateStateIR stateName valueIR) =
  generateAddToAggregateCheck registry action stateName valueIR
generateSingleConstraintCheck registry action (MustWithdrawFromAggregateStateIR stateName valueIR addressIR) =
  generateWithdrawFromAggregateCheck registry action stateName valueIR addressIR
generateSingleConstraintCheck registry _action (MustCheckInstance instanceCheck) =
  generateInstanceCheck registry instanceCheck

-- ============================================================================
-- 3. SPECIFIC CONSTRAINT VALIDATORS
-- ============================================================================

-- | (Internal) Generates the Plutus Tx logic for 'MustBeSignedBy'.
-- This check verifies that a 'PubKeyHash' (sourced from a state field)
-- is present in the transaction's signatories.
generateMustBeSignedByCheck :: [StateInfoIR] -- ^ Mapping between state type and datum type
                            -> PubKeyHashIR -- ^ The source of the required signer's public key hash.
                            -> Q Exp -- ^ TH computation returning the signature check expression.
generateMustBeSignedByCheck _registry (FromActionParamPKH _paramName) =
  [| True |]
generateMustBeSignedByCheck registry (FromStateFieldPKH stateName fieldName) = do
  let datumTypeQ = lookupDatumType registry stateName
  let fieldAccessorName = mkName (unpack fieldName)

  [| let
        signatories = PlutusLedgerApi.V3.txInfoSignatories (scriptContextTxInfo ctx)
        -- The required signer could be in a state being spent OR just referenced
        allInputs = txInfoInputs (scriptContextTxInfo ctx) PlutusTx.Prelude.++ txInfoReferenceInputs (scriptContextTxInfo ctx)

        -- A helper to search a list of inputs for the state and extract the PKH
        findRequiredSigner :: [TxInInfo] -> Maybe PubKeyHash
        findRequiredSigner [] = Nothing
        findRequiredSigner (txInInfo : rest) =
          case PlutusLedgerApi.V3.txOutDatum (PlutusLedgerApi.V3.txInInfoResolved txInInfo) of
            OutputDatum datumData ->
              case (PlutusTx.fromBuiltinData (getDatum datumData) :: Maybe $datumTypeQ) of
                Just stateData -> Just ($(varE fieldAccessorName) stateData)
                Nothing -> findRequiredSigner rest -- Datum was wrong type
            _ -> findRequiredSigner rest -- No inline datum

     in case findRequiredSigner allInputs of
          Just requiredSigner -> requiredSigner `elem` signatories
          Nothing -> traceIfFalse "Constraint failed: Could not find state to extract required signer" False
   |]

-- | (Internal) Generates the Plutus Tx logic for 'InstanceCheckIR'.
generateInstanceCheck :: [StateInfoIR] 
                      -> InstanceCheckIR 
                      -> Q Exp
generateInstanceCheck _ (AddressMatchesParamIR {amprReferenceState, amprParamName}) =
  generateAddressMatchesParamCheck amprReferenceState amprParamName
generateInstanceCheck registry (SameAddressAsReferenceIR {scarReferenceState}) =
  let refStateDatumTypeQ = lookupDatumType registry scarReferenceState
  in [|
    let
      -- Helper to find the input/refInput UTXO for the reference state
      findReferenceInputOutput :: [TxInInfo] -> Maybe TxOut
      findReferenceInputOutput = PlutusTx.Prelude.find findLogic
         where
            findLogic i =
                let out = txInInfoResolved i
                in case txOutDatum out of
                    OutputDatum (Datum d) ->
                        case (fromBuiltinData d :: Maybe $refStateDatumTypeQ) of
                            Just _ -> True
                            Nothing -> False -- Wrong datum type
                    _ -> False -- No datum

      -- Find own input (assuming this check runs in a Spending context)
      ownInputAddress :: Maybe Address
      ownInputAddress = case findOwnInput ctx of
                          Just i -> Just (txOutAddress (txInInfoResolved i))
                          Nothing -> Nothing

      -- Search both regular inputs and reference inputs
      allInputs = txInfoInputs (scriptContextTxInfo ctx) PlutusTx.Prelude.++ txInfoReferenceInputs (scriptContextTxInfo ctx)
      mRefInputOutput = findReferenceInputOutput allInputs

    in case (ownInputAddress, mRefInputOutput) of
         (Just ownAddr, Just refOut) ->
            let refAddr = txOutAddress refOut
            -- Check if own input address matches the reference input address
            in traceIfFalse $(litE (stringL ("Instance Check Failed: SameAddressAsReference failed for " PlutusTx.Prelude.++ unpack scarReferenceState))) (ownAddr PlutusTx.Prelude.== refAddr)
         (Nothing, _) -> traceError "Instance Check Failed: SameAddressAsReference could not find own input"
         (_, Nothing) -> traceError ("Instance Check Failed: SameAddressAsReference could not find reference state: " `appendString` $(litE (stringL (unpack scarReferenceState))))
  |]

-- | (Internal) Generates the Plutus Tx logic ('Q Exp') for the 'MustAddToAggregateStateIR' constraint.
generateAddToAggregateCheck :: [StateInfoIR] -- ^ Mapping between state type and datum type
                            -> ActionIR -- ^ Parent action IR (for value resolution).
                            -> Text -- ^ Name of the aggregate state type.
                            -> FieldValueIR -- ^ IR representing the value to be added.
                            -> Q Exp -- ^ TH computation returning the validation expression.
generateAddToAggregateCheck registry action stateName valueIR = do
    -- Find the Plutus AssetClass associated with the aggregate state via TH.
    assetClassExpValue <- findAggregateAssetClassTH registry stateName
    -- Generate the TH expression for calculating the required value on-chain.
    requiredValueExp <- generatePlutusValueExpression registry action valueIR

    [|
      let
        theAssetClass = $(return assetClassExpValue)
        (PlutusLedgerApi.V1.Value.AssetClass (cs, tn)) = theAssetClass
        -- Evaluate the required value expression at runtime
        requiredValue = $(return requiredValueExp)

        info = scriptContextTxInfo ctx
        outputs = txInfoOutputs info

        -- Check if an output satisfies the conditions
        checkOutput :: TxOut -> Bool
        checkOutput o =
          -- Check 1: Is the output going to a script address?
          case txOutAddress o of
            Address (ScriptCredential _) _ ->
              -- Check 2: Does the output value contain at least the required amount of the specific asset?
              -- Using valueOf >= required amount derived from requiredValue. This assumes requiredValue has only one asset.
              let amountInOut = PlutusLedgerApi.V1.Value.valueOf (txOutValue o) cs tn
                  requiredAmount = PlutusLedgerApi.V1.Value.valueOf requiredValue cs tn
              in amountInOut PlutusTx.Prelude.>= requiredAmount PlutusTx.Prelude.&& requiredAmount PlutusTx.Prelude.> 0
            _ -> False -- Not a script address

        isValid = PlutusTx.Prelude.any checkOutput outputs
      in
        traceIfFalse (stringToBuiltinString ($(litE (stringL ("MustAddToAggregateState failed for state " Prelude.++ unpack stateName))) :: String)
                      `PlutusTx.Prelude.appendString` stringToBuiltinString (" - expected output with asset to a script address not found or value insufficient" :: String))
                     isValid
      |]

-- | (Internal) Generates the Plutus Tx logic ('Q Exp') for the 'MustWithdrawFromAggregateStateIR' constraint.
generateWithdrawFromAggregateCheck :: [StateInfoIR] -- ^ Mapping between state type and datum type
                                   -> ActionIR -- ^ Parent action IR (for value/address resolution).
                                   -> Text -- ^ Name of the aggregate state type.
                                   -> FieldValueIR -- ^ IR representing the value to withdraw.
                                   -> FieldValueIR -- ^ IR representing the destination address.
                                   -> Q Exp -- ^ TH computation returning the validation expression.
generateWithdrawFromAggregateCheck registry action stateName valueIR addressIR = do
    assetClassExpValue <- findAggregateAssetClassTH registry stateName

    -- Generate the address checking function based on the addressIR source.
    destinationCheckFuncExpValue <- case addressIR of
        FromActionParam _ -> [| (\(_ :: PlutusLedgerApi.V3.Address) -> PlutusTx.Prelude.True) |]
        _ -> do
            destinationAddressExpValue <- generateAddressExpression registry action addressIR

            [| (\outputAddr -> outputAddr PlutusTx.Prelude.== $(return destinationAddressExpValue)) |]

    -- Value Check Logic
    valueCheckFuncExpValue <- case valueIR of
        FromActionParam _ ->

            [| \v -> PlutusLedgerApi.V1.Value.valueOf v cs tn PlutusTx.Prelude.> 0 |]
        _ -> do
            withdrawalValueExpValue <- generatePlutusValueExpression registry action valueIR

            [| \v ->
                 let withdrawalValue = $(return withdrawalValueExpValue)
                     withdrawalAmount = PlutusLedgerApi.V1.Value.valueOf withdrawalValue cs tn
                 in PlutusLedgerApi.V1.Value.valueOf v cs tn PlutusTx.Prelude.>= withdrawalAmount PlutusTx.Prelude.&& withdrawalAmount PlutusTx.Prelude.> 0
             |]

    [|
      let
        theAssetClass = $(return assetClassExpValue)
        (AssetClass (cs, tn)) = theAssetClass
        theDestinationCheckFunc :: PlutusLedgerApi.V3.Address -> PlutusTx.Prelude.Bool = $(return destinationCheckFuncExpValue)
        theValueCheckFunc :: PlutusLedgerApi.V3.Value -> PlutusTx.Prelude.Bool = $(return valueCheckFuncExpValue)

        info = scriptContextTxInfo ctx
        inputs = txInfoInputs info
        outputs = txInfoOutputs info

        findInput :: TxInInfo -> PlutusTx.Prelude.Bool
        findInput i =
           case txOutAddress (txInInfoResolved i) of
             Address (ScriptCredential _) _ ->
               theValueCheckFunc (txOutValue (txInInfoResolved i))
             _ -> PlutusTx.Prelude.False

        findOutput :: TxOut -> PlutusTx.Prelude.Bool
        findOutput o =
            theDestinationCheckFunc (txOutAddress o) PlutusTx.Prelude.&&
            theValueCheckFunc (txOutValue o)

        inputExists = PlutusTx.Prelude.any findInput inputs
        outputExists = PlutusTx.Prelude.any findOutput outputs
        isValid = inputExists PlutusTx.Prelude.&& outputExists
      in
        traceIfFalse (stringToBuiltinString ($(litE (stringL ("MustWithdrawFromAggregateState failed for state " Prelude.++ unpack stateName))) :: String)
                      `PlutusTx.Prelude.appendString`
                         (if PlutusTx.Prelude.not inputExists then stringToBuiltinString (" - suitable script input containing the asset not found or value insufficient" :: String)
                          else if PlutusTx.Prelude.not outputExists then stringToBuiltinString (" - suitable output to destination address not found or value insufficient" :: String)
                          else PlutusTx.Prelude.emptyString))
                     isValid
      |]

-- ============================================================================
-- 4. HELPER FUNCTIONS
-- ============================================================================

-- | (Internal) Template Haskell function to derive the Plutus 'AssetClass'
-- for an aggregate state using its 'StateSpec'.
findAggregateAssetClassTH :: [StateInfoIR] -> Text -> Q Exp
findAggregateAssetClassTH registry stateName = do
    -- 1. Compile-time lookup in the registry
    info <- case find (\s -> stateInfoName s Prelude.== stateName) registry of
        Just i -> return i
        Nothing -> fail $ "findAggregateAssetClassTH: State '" Prelude.++ unpack stateName Prelude.++ "' not found in registry."

    -- 2. Extract compile-time constants from IR
    let tokenNameStr = unpack (stateInfoTokenName info)

    -- 3. Determine the policy expression based on the IR
    let policyExp = case stateInfoPolicy info of
          OwnPolicyIR ->
            -- For OwnPolicy, we look up the hash at runtime (on-chain).
            -- Assumes 'ownAddress' is in scope from Logic.hs.
            [| case ownAddress of
                 (PlutusLedgerApi.V3.Address (ScriptCredential (ScriptHash h)) _) ->
                     CurrencySymbol h
                 _ -> PlutusTx.Prelude.traceError "Own Policy: Not a script address"
             |]
          ExternalPolicyIR hex ->
             -- For ExternalPolicy, it's a compile-time constant.
             [| CurrencySymbol (stringToBuiltinByteString $(litE (stringL (unpack hex)))) |]

    -- 4. Generate the final AssetClass construction
    [|
        let
            tn = TokenName (stringToBuiltinByteString $(litE (stringL tokenNameStr)))
            cs = $(policyExp)
        in
            assetClass cs tn
     |]

-- | (Internal) Looks up the datum type name for a given state tag from the registry.
-- Returns a Template Haskell 'Type' corresponding to the datum.
lookupDatumType :: [StateInfoIR] -> Text -> Q Type
lookupDatumType registry stateName = do
    let find' p (x:xs) = if p x then Just x else find' p xs
        find' _ [] = Nothing
    case find' (\info -> stateInfoName info Prelude.== stateName) registry of
        Just info -> conT (mkName (unpack (stateInfoDatumName info)))
        Nothing   -> fail $ "CodeGen Error: State '" Prelude.++ unpack stateName Prelude.++ "' not found in app registry."