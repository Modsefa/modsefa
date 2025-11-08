{-|
Module      : Modsefa.CodeGen.Generation.Logic
Description : Plutus Tx validator logic generation from Modsefa IR.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC (requires TemplateHaskell)

This module takes a value-level 'Modsefa.Core.IR.Types.ValidatorIR' (Intermediate
Representation) and generates the corresponding Plutus Tx Haskell code (using
Template Haskell) that forms the core logic of the validator script.

The generated code typically includes:
- Redeemer dispatch logic (an if/then/else chain based on the action name).
- Validation checks for operations ('CreateOpIR', 'UpdateOpIR', 'DeleteOpIR', 'BatchOpIR') relevant to the validator.
- Validation checks for constraints ('ConstraintIR') defined for each action.
- Handling of reference inputs ('ReferenceOpIR') via let-bindings.
- Minting policy logic if applicable ('TokenIdentified' states with 'OwnPolicy').
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

-- | Logic generation from the Intermediate Representation (IR).
--
-- This module is responsible for taking a `ValidatorIR` and generating
-- the body of the Plutus script for that validator.
module Modsefa.CodeGen.Generation.Logic
  ( generateLogicFromIR
  ) where

import Data.List (nub)
import Data.Text (Text, unpack)
import Language.Haskell.TH
  ( Body(NormalB), Clause(Clause), Dec(FunD, ValD), Exp (LetE), Name, Pat(VarP), Q
  , Quote(newName), conT, integerL, listE, litE, mkName, stringL, varE
  , varP
  )
import Prelude
  ( Bool(..), String, any, elem, filter, foldl, foldr, fromIntegral, length
  , map, return, ($), (++), (<$>)
  )

import PlutusLedgerApi.V3
  ( Address(..), BuiltinByteString, Credential(ScriptCredential), CurrencySymbol(..)
  , Datum(..), OutputDatum(..), Redeemer(..), ScriptContext(..), ScriptHash(..)
  , ScriptInfo(..), TokenName(..), TxInInfo, TxOut, UnsafeFromData(..), Value
  , getValue, mintValueBurned, mintValueMinted, scriptContextTxInfo
  , txInInfoResolved, txInfoInputs, txInfoMint, txInfoOutputs, txInfoReferenceInputs
  , txOutAddress, txOutDatum, unCurrencySymbol
  )
import PlutusLedgerApi.V3.Contexts (findOwnInput)
import PlutusTx (fromBuiltinData)
import PlutusTx.AssocMap (keys, lookup)
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteString)
import PlutusTx.Prelude
  ( Bool(..), Eq, Integer, Maybe(..), all, any, elem, filter
  , head, isJust, length, mapMaybe, not, null, traceError, traceIfFalse, (&&), (==)
  , (||), (>), (++)
  )

import Modsefa.Core.IR.Types
  ( ActionIR(..), BatchOperationIR(..), CollectionConstraintIR(..)
  , FieldValueIR (..), OperationIR(..), ValidatorIR(..)
  )

import Modsefa.CodeGen.Generation.Constraints (generateConstraintChecksForAction)
import Modsefa.CodeGen.Generation.ValueResolution (generateValueExpression)


-- | Generates the main Plutus Tx logic function ('Dec') for a validator from its 'ValidatorIR'.
-- This function will have the type @ParamType -> ScriptContext -> Bool@.
generateLogicFromIR :: Name -- ^ The Template Haskell 'Name' for the function to be generated (e.g., `mkParameterizedMyValidator`).
                    -> ValidatorIR -- ^ The Intermediate Representation of the validator.
                    -> Q Dec -- ^ A Template Haskell computation returning the function definition 'Dec'.
generateLogicFromIR funcName ir = do
  -- Create TH names for variables used in the generated code.
  actionNameVar <- newName "actionName"
  -- Generate the main expression, which decodes the redeemer and dispatches to the correct action logic.
  dispatchExp <- generateActionDispatchChain actionNameVar ir (validatorIRActions ir)
  -- Define the function clause: funcName param ctx = <dispatchExp>
  let funcClause = Clause [VarP (mkName "param"), VarP (mkName "ctx")] (NormalB dispatchExp) []
  -- Return the complete function definition.
  return $ FunD funcName [funcClause]

-- | (Internal) Generates the top-level Plutus Tx 'Exp' that decodes the redeemer ('BuiltinByteString')
-- representing the action name and then uses 'buildIfChain' to create the conditional logic
-- based on this name. It also extracts the validator's own address from the 'ScriptContext'.
generateActionDispatchChain :: Name -- ^ TH 'Name' of the variable holding the decoded action name.
                            -> ValidatorIR -- ^ The validator IR (needed for context, though maybe not directly used here).
                            -> [ActionIR] -- ^ List of actions handled by this validator.
                            -> Q Exp -- ^ TH computation returning the dispatch expression.
generateActionDispatchChain actionNameVar ir actions = do
  -- Build the nested if/then/else structure for action dispatch.
  ifChain <- buildIfChain actionNameVar ir actions
  -- Construct the outer let expression that decodes the redeemer and finds the script's own address.
  [| let
        -- Standard Plutus script boilerplates
        info = scriptContextTxInfo ctx
        Redeemer r = scriptContextRedeemer ctx
        $(varP actionNameVar) = unsafeFromBuiltinData r :: BuiltinByteString

        -- Determine the script's own address based on the script purpose (Spending or Minting).
        ownAddress :: Address
        ownAddress = case scriptContextScriptInfo ctx of
            SpendingScript _ _ ->
                case findOwnInput ctx of
                    Just i -> txOutAddress (txInInfoResolved i)
                    Nothing -> traceError "spending script input not found"
            MintingScript cs ->
                -- The address of a minting policy script is derived from its CurrencySymbol (which is its hash).
                Address (ScriptCredential (ScriptHash (unCurrencySymbol cs))) Nothing
            _ -> traceError "generateActionDispatchChain: Unsupported script purpose"
     in
        -- Execute the generated if/then/else chain for action dispatch.
        $(return ifChain)
    |]

-- | (Internal) Recursively builds a nested Plutus Tx if/then/else expression ('Exp')
-- to dispatch based on the action name. Compares the decoded action name against the
-- name of each 'ActionIR' in the list.
buildIfChain :: Name -- ^ TH 'Name' of the variable holding the decoded action name.
             -> ValidatorIR -- ^ The validator IR (passed down for context).
             -> [ActionIR] -- ^ Remaining list of actions to build branches for.
             -> Q Exp -- ^ TH computation returning the conditional expression.
-- Base case: No actions left. If the redeemer didn't match any known action, trigger an error.
buildIfChain _ _ [] =
  [| traceError "buildIfChain: Redeemer does not match any known action" |]
buildIfChain actionNameVar ir (action : rest) = do
  let actionNameText = actionIRName action
  -- Generate the Plutus Tx expression representing the validation logic for this specific action.
  actionLogic <- generateActionLogic ir action
  -- Recursively build the 'else' part of the chain for the remaining actions.
  restOfChain <- buildIfChain actionNameVar ir rest

  -- Create the condition: actionNameVar == "ActionNameLiteral"
  let actionNameString = unpack actionNameText
  let condition =
        [| $(varE actionNameVar) PlutusTx.Prelude.== stringToBuiltinByteString ($(litE (stringL actionNameString)) :: String) |]

  -- Create the 'then' branch: traceIfFalse "Validation failed for ActionName" (actionLogicExp)
  let successBranch =
        [| traceIfFalse $(litE (stringL ("Validation failed for action: " Prelude.++ actionNameString))) $(return actionLogic) |]

  -- Combine into the if/then/else expression.
  [| if $(condition) then $(successBranch) else $(return restOfChain) |]

-- | (Internal) Generates the complete Plutus Tx validation logic ('Exp') for a single 'ActionIR'.
-- Combines the validation expressions generated for the action's constraints and operations.
-- Also handles setting up let-bindings for resolved reference inputs.
generateActionLogic :: ValidatorIR -- ^ The validator IR.
                    -> ActionIR -- ^ The specific action IR to generate logic for.
                    -> Q Exp -- ^ TH computation returning the combined validation expression.
generateActionLogic ir action = do
  -- Generate the expression for checking all constraints.
  constraintChecksExp <- generateConstraintChecksForAction action
  -- Generate the expression for checking relevant operations (based on managed states).
  operationChecksExp <- generateOperationChecks ir action
  -- Generate expression for checking minting/burning if applicable.
  mintingChecksExp <- generateMintingChecks ir (actionIROperations action)

  -- Find all unique states referenced via 'ReferenceOpIR' within this action.
  let referencedStates = collectReferencedStates action

  -- Generate the 'let' bindings (as TH 'Dec's) needed to look up and decode reference inputs.
  refInputBindings <- generateRefInputBindings referencedStates

  -- Construct the final expression:
  -- let <ref input bindings>
  -- in <constraint checks> && <operation checks depending on script type>
  LetE refInputBindings <$> [|
        let
          constraintChecksResult = $(return constraintChecksExp)
          operationChecksResult =
            case scriptContextScriptInfo ctx of
              SpendingScript _ _ -> $(return operationChecksExp)
              MintingScript _  -> $(return mintingChecksExp) && $(return operationChecksExp)
              _                -> True
        in
          constraintChecksResult && operationChecksResult
      |]

-- | (Internal) Generates the Plutus Tx validation logic ('Exp') for all operations within an 'ActionIR'
-- that are relevant to the current validator (i.e., operate on states listed in 'validatorIRManagedStates').
generateOperationChecks :: ValidatorIR -> ActionIR -> Q Exp
generateOperationChecks validatorIR action =
  let
    managedStates = validatorIRManagedStates validatorIR
    allOps = actionIROperations action
    relevantOps = Prelude.filter (\op -> getOpStateName op `Prelude.elem` managedStates) allOps
    checks :: [Q Exp]
    checks = Prelude.map (generateSingleOperationCheck action) relevantOps
  in
    foldr (\a b -> [| $a && $b |]) [|True|] checks

-- | Generates validation for a single operation.
generateSingleOperationCheck :: ActionIR -> OperationIR -> Q Exp

generateSingleOperationCheck action (CreateOpIR stateName fields) =
  let
    stateTypeName = conT (mkName (unpack stateName))
    -- Get the checks for constant fields
    constantChecks = generateFieldValidationChecks action fields
  in
  [| let
        outputsToSelf = PlutusTx.Prelude.filter (\o -> txOutAddress o PlutusTx.Prelude.== ownAddress) (txInfoOutputs (scriptContextTxInfo ctx))

        -- A function to check a single output
        isValidOutput :: TxOut -> Bool
        isValidOutput txOut = case txOutDatum txOut of
            OutputDatum (Datum d) ->
                case (fromBuiltinData d :: Maybe $stateTypeName) of
                    Just outputDatum -> $(
                        -- Combine all constant checks with AND
                        let allChecks = foldr (\a b -> [| $a && $b |]) [|True|] constantChecks
                        in [| $allChecks |]
                      )
                    Nothing -> False
            _ -> False
     in
        -- Check if *any* of the outputs to self is a valid one
        traceIfFalse $(litE (stringL ("CreateOp failed: Could not find a valid created output for state " Prelude.++ unpack stateName)))
            (PlutusTx.Prelude.any isValidOutput outputsToSelf)
    |]
generateSingleOperationCheck action (UpdateOpIR stateName fields) =
    let
        stateTypeName = conT (mkName (unpack stateName))
        preservedFields = [fieldName | (fieldName, FromInputField _) <- fields]
        -- Get the checks for constant fields
        fieldChecks = generateFieldValidationChecks action fields
        preservationChecks = Prelude.map (\fieldName ->
            let fieldAccessor = varE (mkName (unpack fieldName)) in
            [| $fieldAccessor inputDatum PlutusTx.Prelude.== $fieldAccessor outputDatum |]
            ) preservedFields
    in
    [|
        case PlutusLedgerApi.V3.scriptContextScriptInfo ctx of
            PlutusLedgerApi.V3.SpendingScript _ _ ->
                let
                    info = PlutusLedgerApi.V3.scriptContextTxInfo ctx
                    inputsFromSelf = PlutusTx.Prelude.filter (\i -> PlutusLedgerApi.V3.txOutAddress (PlutusLedgerApi.V3.txInInfoResolved i) PlutusTx.Prelude.== ownAddress) (PlutusLedgerApi.V3.txInfoInputs info)

                    shapeIsValid = PlutusTx.Prelude.length inputsFromSelf PlutusTx.Prelude.== 1

                    inputDatum :: $stateTypeName
                    inputDatum = case PlutusLedgerApi.V3.txOutDatum (PlutusLedgerApi.V3.txInInfoResolved (PlutusTx.Prelude.head inputsFromSelf)) of
                        PlutusLedgerApi.V3.OutputDatum (PlutusLedgerApi.V3.Datum d) ->
                            case (PlutusTx.fromBuiltinData d :: Maybe $stateTypeName) of
                                Just dat -> dat
                                Nothing  -> PlutusTx.Prelude.traceError "Could not decode input datum for update"
                        _ -> PlutusTx.Prelude.traceError "Update input missing inline datum"

                    outputCandidates :: [$stateTypeName]
                    outputCandidates =
                        let
                            outputsToSelf = PlutusTx.Prelude.filter (\o -> PlutusLedgerApi.V3.txOutAddress o PlutusTx.Prelude.== ownAddress) (PlutusLedgerApi.V3.txInfoOutputs info)
                            extractDatum :: PlutusLedgerApi.V3.TxOut -> Maybe $stateTypeName
                            extractDatum o = case PlutusLedgerApi.V3.txOutDatum o of
                                PlutusLedgerApi.V3.OutputDatum (PlutusLedgerApi.V3.Datum d) -> (PlutusTx.fromBuiltinData d :: Maybe $stateTypeName)
                                _ -> Nothing
                        in
                            PlutusTx.Prelude.mapMaybe extractDatum outputsToSelf

                    -- Check if any output is a valid target
                    anyOutputIsValid = PlutusTx.Prelude.any (\(outputDatum :: $stateTypeName) ->
                        let
                            allPreservationChecks = $(foldr (\a b -> [| $a && $b |]) [|True|] preservationChecks)
                            allConstantChecks = $(foldr (\a b -> [| $a && $b |]) [|True|] fieldChecks)
                        in
                            allPreservationChecks && allConstantChecks
                        ) outputCandidates

                in
                    PlutusTx.Prelude.traceIfFalse "UpdateOp failed: Invalid shape (expected 1 script input)" shapeIsValid &&
                    PlutusTx.Prelude.traceIfFalse "UpdateOp failed: Could not find a valid output satisfying all constraints" anyOutputIsValid
            _ -> PlutusTx.Prelude.True
    |]
generateSingleOperationCheck _action (DeleteOpIR _ ) =
    [| let
          inputsFromSelf = PlutusTx.Prelude.filter (\i -> txOutAddress (txInInfoResolved i) PlutusTx.Prelude.== ownAddress) (txInfoInputs (scriptContextTxInfo ctx))
          outputsToSelf = PlutusTx.Prelude.filter (\o -> txOutAddress o PlutusTx.Prelude.== ownAddress) (txInfoOutputs (scriptContextTxInfo ctx))
       in
          traceIfFalse "DeleteOp failed: Expected at least one input and no outputs from/to the script"
            (not (PlutusTx.Prelude.null inputsFromSelf) && PlutusTx.Prelude.null outputsToSelf)
      |]
generateSingleOperationCheck _action (ReferenceOpIR stateName label) =
    [| let
          inputs = txInfoInputs (scriptContextTxInfo ctx)
          refInputs = txInfoReferenceInputs (scriptContextTxInfo ctx)
          isTargetState :: TxInInfo -> Bool
          isTargetState txInInfo = case txOutDatum (txInInfoResolved txInInfo) of
              OutputDatum (Datum d) -> isJust (fromBuiltinData d :: Maybe $(conT (mkName (unpack stateName))))
              _ -> False
       in
          traceIfFalse $(litE (stringL ("ReferenceOp failed: Could not find reference input '" Prelude.++ unpack label Prelude.++ "'"))) (PlutusTx.Prelude.any isTargetState inputs || PlutusTx.Prelude.any isTargetState refInputs)
      |]
generateSingleOperationCheck _action (BatchOpIR (BatchCreateIR stateName _) _ constraints) =
    [| let
          outputsToSelf = PlutusTx.Prelude.filter (\o -> txOutAddress o PlutusTx.Prelude.== ownAddress) (txInfoOutputs (scriptContextTxInfo ctx))
          allDatumsAreCorrect = all
            (\output -> case txOutDatum output of
              OutputDatum (Datum d) -> isJust (fromBuiltinData d :: Maybe $(conT (mkName (unpack stateName))))
              _ -> False
            ) outputsToSelf
          uniquenessChecks = $(generateUniquenessChecks stateName constraints)
       in
          traceIfFalse "BatchCreateOp failed: Expected at least one output to self" (not (null outputsToSelf)) &&
          traceIfFalse "BatchCreateOp failed: Not all outputs had the correct datum type" allDatumsAreCorrect &&
          uniquenessChecks outputsToSelf
      |]
generateSingleOperationCheck _action (BatchOpIR (BatchDeleteIR _) _ _) =
    [| let
          inputsFromSelf = PlutusTx.Prelude.filter (\i -> txOutAddress (txInInfoResolved i) PlutusTx.Prelude.== ownAddress) (txInfoInputs (scriptContextTxInfo ctx))
          outputsToSelf = PlutusTx.Prelude.filter (\o -> txOutAddress o PlutusTx.Prelude.== ownAddress) (txInfoOutputs (scriptContextTxInfo ctx))
       in
          traceIfFalse "BatchDeleteOp failed: Expected at least one input and no outputs from/to the script"
            (not (null inputsFromSelf) && null outputsToSelf)
      |]

-- | (Internal) Generates the on-chain validation logic ('Exp') for 'CollectionConstraintIR's,
-- specifically 'MustHaveUniqueFieldIR'. Returns a function of type @[TxOut] -> Bool@.
generateUniquenessChecks :: Text -- ^ State name (for datum decoding).
                         -> [CollectionConstraintIR] -- ^ List of collection constraints.
                         -> Q Exp -- ^ TH Expression: \[TxOut] -> Bool
generateUniquenessChecks stateName constraints = do
    -- Generate check functions for each constraint.
    let checks = Prelude.map (generateSingleUniquenessCheck stateName) constraints
    [| \outputs -> all (\check -> check outputs) $(listE checks) |]

-- | (Internal) Generates the check function ('Exp') for a single 'MustHaveUniqueFieldIR'.
-- The generated function takes the list of outputs and verifies uniqueness of the specified field.
generateSingleUniquenessCheck :: Text -- ^ State name.
                              -> CollectionConstraintIR -- ^ The uniqueness constraint.
                              -> Q Exp -- ^ TH Expression: [TxOut] -> Bool
generateSingleUniquenessCheck stateName (MustHaveUniqueFieldIR fieldName) =
    let
      stateTypeName = conT (mkName (unpack stateName))
      fieldAccessor = varE (mkName (unpack fieldName))
      fieldType = conT ''PlutusTx.Prelude.Integer
    in
    [| \outputs ->
        let
          -- On-chain helper to extract the specific field from all output datums.
          extractFields :: [TxOut] -> [$fieldType]
          extractFields [] = []
          extractFields (o:os) =
            case txOutDatum o of
              OutputDatum (Datum d) ->
                case (fromBuiltinData d :: Maybe $stateTypeName) of
                  -- The field accessor is applied, and the result must match the concrete `fieldType`
                  Just datum -> $fieldAccessor datum : extractFields os
                  Nothing    -> extractFields os
              _ -> extractFields os

          -- On-chain uniqueness check.
          isUnique :: PlutusTx.Prelude.Eq a => [a] -> PlutusTx.Prelude.Bool
          isUnique [] = PlutusTx.Prelude.True
          isUnique (x:xs) = not (PlutusTx.Prelude.elem x xs) && isUnique xs
        in
          -- The call to isUnique works because `extractFields` returns a concrete list type.
          isUnique (extractFields outputs)
    |]

-- | (Internal) Helper function to extract the state name from any 'OperationIR'.
-- Returns empty string for unrecognized operations (shouldn't happen).
getOpStateName :: OperationIR -> Text
getOpStateName (CreateOpIR name _) = name
getOpStateName (UpdateOpIR name _) = name
getOpStateName (DeleteOpIR name)   = name
getOpStateName (BatchOpIR (BatchCreateIR name _) _ _) = name
getOpStateName (BatchOpIR (BatchDeleteIR name) _ _) = name
getOpStateName _ = ""

isMintingOp :: OperationIR -> Bool
isMintingOp (CreateOpIR _ _) = True
isMintingOp (DeleteOpIR _) = True
isMintingOp (BatchOpIR (BatchCreateIR _ _) _ _) = True
isMintingOp (BatchOpIR (BatchDeleteIR _) _ _) = True
isMintingOp _ = False

-- | (Internal) Generates the Plutus Tx validation logic ('Exp') specific to minting policies.
-- Checks that the correct quantity of the expected token name(s) is minted or burned, matching
-- the 'CreateOpIR'/'DeleteOpIR'/'BatchOpIR' operations relevant to the current validator.
generateMintingChecks :: ValidatorIR -> [OperationIR] -> Q Exp
generateMintingChecks validatorIR allOps = do
    let managedStates = validatorIRManagedStates validatorIR
    -- Filter operations relevant to this validator AND potentially involving minting/burning.
    let relevantOps = Prelude.filter (\op -> getOpStateName op `Prelude.elem` managedStates) allOps
    let mintingOps = Prelude.filter isMintingOp relevantOps
    let checks = Prelude.map opToCheck mintingOps
    let allChecksExp = Prelude.foldl (\acc chk -> [| $(acc) && $(chk) |]) [|True|] checks

    -- Determine if this is a mint or a burn action to select the right function.
    let isBurn = Prelude.any isDeleteOp relevantOps
        mintCheckFunc = if isBurn
                          then 'PlutusLedgerApi.V3.mintValueBurned
                          else 'PlutusLedgerApi.V3.mintValueMinted

    if null mintingOps then
        [| True |]
    else
        [|
            let
                info = scriptContextTxInfo ctx

                ownCS :: PlutusLedgerApi.V3.CurrencySymbol
                ownCS = case scriptContextScriptInfo ctx of
                            MintingScript cs -> cs
                            _ -> traceError "FATAL: minting check called in non-minting context"

                -- Use the correct function (minted or burned) based on the action type
                mintedValue :: PlutusLedgerApi.V3.Value
                mintedValue = $(varE mintCheckFunc) (txInfoMint info)

                checkMint :: Bool
                checkMint = case PlutusTx.AssocMap.lookup ownCS (getValue mintedValue) of
                    Nothing -> traceError "Minting check failed: ownCS lookup returned Nothing."
                    Just tokenMap ->
                        let
                          correctTokenCount = PlutusTx.Prelude.length (PlutusTx.AssocMap.keys tokenMap) PlutusTx.Prelude.==  $(litE (integerL (fromIntegral (Prelude.length mintingOps))))
                        in
                          correctTokenCount && $(allChecksExp)
            in
                traceIfFalse "Minting validation failed" checkMint
        |]
  where
    -- Helper predicate: Is the operation a Delete or BatchDelete?
    isDeleteOp :: OperationIR -> Bool
    isDeleteOp (DeleteOpIR _) = True
    isDeleteOp (BatchOpIR (BatchDeleteIR _) _ _) = True
    isDeleteOp _ = False

    opToCheck :: OperationIR -> Q Exp
    opToCheck op =
      let tokenNameExp = [| TokenName (stringToBuiltinByteString ($(litE (stringL (unpack (getOpStateName op)))) :: String)) |]
      in case op of
          CreateOpIR _ _ -> [| PlutusTx.AssocMap.lookup $(tokenNameExp) tokenMap PlutusTx.Prelude.== Just 1 |]
          DeleteOpIR _ -> [| PlutusTx.AssocMap.lookup $(tokenNameExp) tokenMap PlutusTx.Prelude.== Just 1 |]
          BatchOpIR (BatchCreateIR _ _) _ _ -> [| case PlutusTx.AssocMap.lookup $(tokenNameExp) tokenMap of Just qty -> qty > 0; _ -> False |]
          BatchOpIR (BatchDeleteIR _) _ _ -> [| case PlutusTx.AssocMap.lookup $(tokenNameExp) tokenMap of Just qty -> qty > 0; _ -> False |]
          _ -> [| True |]

-- | (Internal) Determines if a 'FieldValueIR' represents a value that can be directly
-- validated or calculated on-chain within the validator script. Excludes sources
-- like action parameters or input fields which aren't available directly.
canValidateOnChain :: FieldValueIR -> Bool
canValidateOnChain (FromStateField _ _) = True
canValidateOnChain (AddValueIR v1 v2)      = canValidateOnChain v1 && canValidateOnChain v2
canValidateOnChain (SubtractValueIR v1 v2) = canValidateOnChain v1 && canValidateOnChain v2
canValidateOnChain (MultiplyValueIR v1 v2) = canValidateOnChain v1 && canValidateOnChain v2
canValidateOnChain (DivideValueIR v1 v2)   = canValidateOnChain v1 && canValidateOnChain v2
canValidateOnChain CurrentTimeIR           = True
canValidateOnChain (FromInt _)             = True
canValidateOnChain (FromEnum _ _)          = True
canValidateOnChain _                       = False

-- | (Internal) Generates a list of Plutus Tx boolean expressions ('Q Exp') for validating
-- the fields of a *created* or *updated* output datum ('outputDatum' must be in scope).
-- Only includes checks for fields whose 'FieldValueIR' source is verifiable on-chain
-- (see 'canValidateOnChain').
generateFieldValidationChecks :: ActionIR -> [(Text, FieldValueIR)] -> [Q Exp]
generateFieldValidationChecks action fields =
    let
        validatableFields = Prelude.filter (\(_, fv) -> canValidateOnChain fv) fields

        checks = Prelude.map (\(fieldName, fieldValue) ->
            let
                fieldAccessor = varE (mkName (unpack fieldName))
                expectedValueExp = generateValueExpression action fieldValue
            in
            [| $fieldAccessor outputDatum PlutusTx.Prelude.== $(expectedValueExp) |]
          ) validatableFields
    in
        checks

-- | (Internal) Collects all unique referenced states (Label, StateName) from an 'ActionIR'.
collectReferencedStates :: ActionIR -> [(Text, Text)] -- List of (Label, StateName)
collectReferencedStates action =
  -- Use Data.List.nub to remove duplicates after mapping/filtering.
  Data.List.nub $ mapMaybe getRefDetails (actionIROperations action)
  where
    -- Extracts label and state name if the operation is ReferenceOpIR.
    getRefDetails :: OperationIR -> Maybe (Text, Text)
    getRefDetails (ReferenceOpIR stateName label) = Just (label, stateName)
    getRefDetails _                               = Nothing

-- | (Internal) Generates Template Haskell 'let' bindings ('Dec's) to look up and decode
-- reference inputs based on the list collected by 'collectReferencedStates'.
-- Each binding looks like: @ref_myLabel = findReferenceInputOutput [...] :: Maybe MyStateType@
generateRefInputBindings :: [(Text, Text)] -- ^ List of (Label, StateName).
                         -> Q [Dec] -- ^ TH computation returning list of 'ValD' declarations.
generateRefInputBindings [] = return [] -- Base case: No references, no bindings.
generateRefInputBindings ((label, stateName) : refs) = do
  -- Create the variable name for the binding (e.g., ref_config).
  let datumVarName = mkName ("ref_" Prelude.++ unpack label)
  let stateTypeName = conT (mkName (unpack stateName))
  -- Generate the findRefInput function application for this specific state
  let findLogicQExp = [|
        let
            findRefInput :: [TxInInfo] -> Maybe $stateTypeName
            findRefInput [] = Nothing
            findRefInput (i:is) =
                case txOutDatum (txInInfoResolved i) of
                    OutputDatum (Datum d) ->
                        -- Attempt to decode, return Just if successful, else recurse
                        case (fromBuiltinData d :: Maybe $stateTypeName) of
                            Just datum -> Just datum
                            Nothing    -> findRefInput is
                    _ -> findRefInput is -- No datum or wrong datum type, recurse

            -- Search all inputs (regular + reference)
            allInputs = txInfoInputs (scriptContextTxInfo ctx) PlutusTx.Prelude.++ txInfoReferenceInputs (scriptContextTxInfo ctx)
        in
            findRefInput allInputs
        |]
  findLogicExp <- findLogicQExp
  -- Create the ValD (value declaration) for the let binding
  let binding = ValD (VarP datumVarName) (NormalB findLogicExp) []
  -- Recursively generate bindings for the rest of the refs
  restBindings <- generateRefInputBindings refs
  return (binding : restBindings)