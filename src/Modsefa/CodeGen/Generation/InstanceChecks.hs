{-|
Module      : Modsefa.CodeGen.Generation.InstanceChecks
Description : Plutus Tx code generation for Modsefa instance consistency checks.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC (requires TemplateHaskell)

This module provides Template Haskell functions specifically for generating
the Plutus Tx code fragments that implement on-chain instance consistency checks.
These checks correspond to the 'Modsefa.Core.IR.Types.InstanceCheckIR' data type,
which is generated during IR compilation ('Modsefa.Core.IR.Compiler') when actions
involve references across validator boundaries, particularly when parameter derivations
are involved.

The generated checks ensure that referenced state UTxOs belong to the same logical
application instance as the validator script currently executing.
-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Plutus Tx code generation specifically for instance consistency checks.
module Modsefa.CodeGen.Generation.InstanceChecks
  ( generateAddressMatchesParamCheck
  ) where

import Data.Text (Text, unpack)
import Language.Haskell.TH(Exp, Q, conT, litE, stringL, mkName)
import Prelude(Maybe(..))

import PlutusLedgerApi.V3
  ( Address(..), Datum(..), OutputDatum(..), ScriptContext(..), TxInInfo, TxOut
  , scriptContextTxInfo, txInInfoResolved, txInfoInputs, txInfoReferenceInputs
  , txOutAddress, txOutDatum
  )
import PlutusTx (fromBuiltinData)
import PlutusTx.Prelude
  ( Eq((==)), appendString, traceError, traceIfFalse, (==), (++)
  )

-- | Generates the Plutus Tx code ('Q Exp') for the 'Modsefa.Core.IR.Types.AddressMatchesParamIR' check.
-- This check is used within a *derived* validator script (e.g., CouponValidator derived from ServiceValidator).
-- It verifies that the address of the UTXO containing the referenced state's datum (e.g., ServiceConfig)
-- matches the address passed as a parameter ('paramName') to the current derived validator script.
-- Assumes 'param' (the validator parameter value) and 'ctx' ('ScriptContext') are in scope where the generated code is spliced.
generateAddressMatchesParamCheck :: Text -- ^ The datum type name ('Modsefa.Core.Foundation.Types.GetStateName') of the referenced state.
                                 -> Text -- ^ The name of the 'Address' parameter in the current validator's 'Modsefa.Core.Foundation.Types.Params'.
                                 -> Q Exp -- ^ TH computation returning the validation expression ('Bool').
generateAddressMatchesParamCheck referenceStateDatumName _paramName = do
  -- Get the TH 'Type' for the referenced state's datum record.
  let refStateDatumTypeName = conT (mkName (unpack referenceStateDatumName))
  [|
    let
      -- Helper to find the input/refInput UTXO containing the specified datum type
      findReferenceInputOutput :: [TxInInfo] -> Maybe TxOut
      findReferenceInputOutput [] = Nothing
      findReferenceInputOutput (i:is) =
        let out = txInInfoResolved i
        in case txOutDatum out of
             OutputDatum (Datum d) ->
               -- Try to decode datum as the specified reference state datum type
               case (fromBuiltinData d :: Maybe $refStateDatumTypeName) of
                 Just _ -> Just out -- Return the resolved TxOut
                 Nothing -> findReferenceInputOutput is -- Wrong datum type
             _ -> findReferenceInputOutput is -- No datum

      -- Get the expected address from the validator's parameter.
      -- Assumes 'param' holds the Address.
      expectedAddress :: Address
      expectedAddress = param

      -- Search both regular inputs and reference inputs
      allInputs = txInfoInputs (scriptContextTxInfo ctx) PlutusTx.Prelude.++ txInfoReferenceInputs (scriptContextTxInfo ctx)
      mRefInputOutput = findReferenceInputOutput allInputs

    in case mRefInputOutput of
         Nothing -> traceError ("Instance Check Failed: Could not find input/refInput for state datum: " `appendString` $(litE (stringL (unpack referenceStateDatumName))))
         Just refInputTxOut ->
           let actualAddress = txOutAddress refInputTxOut
           in traceIfFalse ("Instance Check Failed: Address mismatch for referenced state " `appendString` $(litE (stringL (unpack referenceStateDatumName)))) (actualAddress == expectedAddress)
    |]