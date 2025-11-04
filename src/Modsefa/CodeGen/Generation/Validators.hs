{-|
Module      : Modsefa.CodeGen.Generation.Validators
Description : Template Haskell code generation for Modsefa validator scripts.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC (requires TemplateHaskell)

This module provides the primary Template Haskell (TH) functions for generating
Plutus Tx validator code from Modsefa application specifications ('AppSpec').
The main entry point is 'generateAllValidatorsForApp', which takes an application
type, compiles its specification into an Intermediate Representation (IR), and then
generates the necessary Haskell functions (including the parameterized validator logic
and the required Plutus Tx wrapper) for each validator defined in the 'AppSpec'.

It orchestrates the generation process by delegating specific tasks:
- Compiling the specification to IR ('Modsefa.Core.IR.Compiler.compileIR').
- Generating the core validation logic from the IR ('Modsefa.CodeGen.Generation.Logic.generateLogicFromIR').
- Generating type signatures.
- Generating the standard Plutus Tx wrapper function.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Main entry points for validator code generation
--
-- This module provides the primary Template Haskell functions for generating
-- validator code from app specifications. It orchestrates the generation process
-- by delegating to specialized modules for different aspects of code generation.
module Modsefa.CodeGen.Generation.Validators
  (  generateAllValidatorsForApp
  ) where

import Data.List (concat)
import Data.Text (Text, unpack)
import Data.Typeable (TypeRep, splitTyConApp, typeRep)
import GHC.TypeLits (KnownSymbol)
import Language.Haskell.TH
  ( Body(NormalB), Clause(Clause), Dec(FunD, PragmaD, SigD), Exp(AppE, VarE)
  , Inline(Inlinable), Name, Pat(VarP), Phases(AllPhases), Pragma(InlineP)
  , RuleMatch(FunLike), Q, Type(AppT, ArrowT, ConT, TupleT), mkName
  )
import Prelude 
  ( Maybe(..), Traversable(..), fail, foldl, map, pure, return, show, ($), (++)
  , (==)
  )

import PlutusLedgerApi.V3 (BuiltinData, ScriptContext, unsafeFromBuiltinData)
import PlutusTx.Prelude (Bool, BuiltinUnit, check)

import Modsefa.Core.Foundation 
  ( ActionTransitions, AppInstanceParameters, AppSpec, AppStates, InitialAppState
  , ParameterDerivations, ValidatorSpec, Validators
  )
import Modsefa.Core.IR.Compiler (compileIR)
import Modsefa.Core.IR.Types (AppIR(..), ValidatorIR(..))
import Modsefa.Core.Singletons
  ( AutoSingletonActionTransitionList, AutoSingletonAppInstanceParams
  , AutoSingletonAppStateList, AutoSingletonInitialAppState
  , AutoSingletonParamDerivationList, AutoSingletonValidators, SAppSpec(SAppSpec)
  , SParamList(..), SValidator(..), SValidatorList(..), SomeValidator(..)
  , autoSingletonFull, getValidatorNameFromSingleton  
  )

import Modsefa.CodeGen.Generation.Logic (generateLogicFromIR)


-- ============================================================================
-- 1. MAIN ENTRY POINTS
-- ============================================================================

-- | Generates all validator-related Haskell declarations ('Dec') for a given
-- application type @app@ using Template Haskell.
--
-- This function is intended to be used via TH splices in your code, like:
-- @$(generateAllValidatorsForApp \@MyApplication)@
--
-- It performs the following steps:
-- 1. Uses 'Modsefa.Core.Singletons.Auto.autoSingletonFull' to get the value-level 'SAppSpec' singleton.
-- 2. Compiles the 'SAppSpec' into an 'AppIR' using 'Modsefa.Core.IR.Compiler.compileIR'.
-- 3. Iterates through each 'ValidatorIR' in the 'AppIR'.
-- 4. For each 'ValidatorIR', calls 'generateValidatorFromIR' to produce the Haskell declarations
--    (type signatures, function definitions, and INLINE pragmas for both the parameterized logic
--    function and its Plutus Tx wrapper).
-- 5. Concatenates and returns all generated declarations.
--
-- Requires various 'AutoSingleton*' constraints on the application type @app@ to
-- automatically derive the necessary specification information.
generateAllValidatorsForApp :: forall app.
  ( AppSpec app
  , KnownSymbol (InitialAppState app)
  , AutoSingletonValidators (Validators app)
  , AutoSingletonAppStateList (AppStates app)
  , AutoSingletonInitialAppState (InitialAppState app)
  , AutoSingletonActionTransitionList (ActionTransitions app)
  , AutoSingletonAppInstanceParams (AppInstanceParameters app)
  , AutoSingletonParamDerivationList (ParameterDerivations app)
  ) => Q [Dec] -- ^ A Template Haskell computation returning a list of declarations.
generateAllValidatorsForApp = do
  -- Automatically derive the full application specification singleton.
  let appSpec' = autoSingletonFull @app
      -- The app name isn't encoded in the types, provide a stub or potentially add later.
      appName = ""
      -- Compile the singleton spec into the Intermediate Representation (IR).
      appIR = compileIR appName appSpec'
      (SAppSpec sValidators _ _ _ _ _) = appSpec'

  -- Generate declarations for each validator defined in the IR.
  -- 'traverse' maps 'generateValidatorFromIR' over the list within the Q monad.
  decs <- traverse (generateValidatorFromIR sValidators) (appIRValidators appIR)
  return (concat decs)

-- | (Internal) Generates the full set of Haskell declarations ('Dec') for a single validator
-- based on its 'ValidatorIR' representation and the original 'SValidatorList'.
generateValidatorFromIR :: SValidatorList vs -- ^ Original validator singletons (needed for parameter types).
                        -> ValidatorIR -- ^ The IR for the specific validator to generate code for.
                        -> Q [Dec] -- ^ TH computation returning the generated declarations.
generateValidatorFromIR sValidators ir = do
  -- Find the original 'SValidator' singleton corresponding to the IR's name.
  -- This is needed to reconstruct the parameter types for the type signature.
  case findValidatorSingletonByName sValidators (validatorIRName ir) of
    Nothing -> fail $ "InternalCodeGenError: Could not find validator singleton for: " ++ unpack (validatorIRName ir)
    Just (SomeValidator (validator :: SValidator v)) -> do
      -- Generate the type signatures for the logic and wrapper functions.
      (logicSig, wrapperSig) <- generateValidatorTypeSignatures validator
      -- Generate the function definitions (bodies) and their INLINE pragmas.
      (logicFunc, wrapperFunc, logicPragma, wrapperPragma) <- generateFunctionsFromIR ir

      -- Return all generated declarations in the standard order.
      return [logicPragma, logicSig, logicFunc, wrapperPragma, wrapperSig, wrapperFunc]

-- ============================================================================
-- 2. TYPE SIGNATURE GENERATION
-- ============================================================================

-- | (Internal) Generates the Template Haskell 'Dec' values for the type signatures
-- of the parameterized validator logic function and its Plutus Tx wrapper.
generateValidatorTypeSignatures :: forall v. (ValidatorSpec v)
                                => SValidator v -- ^ The singleton for the validator.
                                -> Q (Dec, Dec) -- ^ TH computation returning (logicSigDec, wrapperSigDec).
generateValidatorTypeSignatures validator@(SValidator params _ _ _) = do
  let validatorName = getValidatorNameFromSingleton validator
  -- Construct the conventional names for the generated functions.
  let logicName = mkName ("mkParameterized" ++ unpack validatorName)
  let wrapperName = mkName ("mkWrappedParameterized" ++ unpack validatorName)

  -- Build the Template Haskell 'Type' representing the validator's parameter type
  -- (e.g., () or (TxOutRef, (Address, ...)) ).
  paramThType <- buildParamThType params

  let logicSig = SigD logicName (AppT (AppT ArrowT paramThType)
                                     (AppT (AppT ArrowT (ConT ''ScriptContext)) (ConT ''Bool)))

  let wrapperSig = SigD wrapperName (AppT (AppT ArrowT (ConT ''BuiltinData))
                                         (AppT (AppT ArrowT (ConT ''BuiltinData)) (ConT ''BuiltinUnit)))

  return (logicSig, wrapperSig)

-- ============================================================================
-- 3. FUNCTION ORCHESTRATION
-- ============================================================================

-- | (Internal) Generates the Template Haskell 'Dec' values for the function definitions
-- (bodies) and 'INLINE' pragmas for the validator logic and wrapper functions.
generateFunctionsFromIR :: ValidatorIR -- ^ The IR for the validator.
                        -> Q (Dec, Dec, Dec, Dec) -- ^ TH: (logicFuncDec, wrapperFuncDec, logicPragmaDec, wrapperPragmaDec).
generateFunctionsFromIR ir = do
  let vName = validatorIRName ir
  -- Construct the function names.
  let funcName = mkName ("mkParameterized" ++ unpack vName)
  let wrapperName = mkName ("mkWrappedParameterized" ++ unpack vName)

  -- Delegate the generation of the core validation logic (if/then/else based on redeemer).
  logicFunc <- generateLogicFromIR funcName ir

  -- Generate the standard Plutus Tx wrapper function.
  wrapperFunc <- generateWrapperFunction funcName wrapperName

  -- Generate INLINE pragmas, crucial for Plutus Tx optimization.
  let logicPragma = PragmaD (InlineP funcName Inlinable FunLike AllPhases)
  let wrapperPragma = PragmaD (InlineP wrapperName Inlinable FunLike AllPhases)

  return (logicFunc, wrapperFunc, logicPragma, wrapperPragma)

-- | (Internal) Generates the standard Plutus Tx wrapper function definition.
-- This wrapper takes 'BuiltinData' arguments (parameter, datum, context),
-- uses 'unsafeFromBuiltinData' to decode them, calls the parameterized logic function,
-- and uses 'check' to ensure the result is 'True' (throwing an error otherwise).
generateWrapperFunction :: Name -- ^ Name of the parameterized logic function.
                        -> Name -- ^ Name of the wrapper function to generate.
                        -> Q Dec -- ^ TH computation returning the function definition 'Dec'.
generateWrapperFunction logicName wrapperName = do
  return $ FunD wrapperName
    [Clause [VarP (mkName "param"), VarP (mkName "ctx")]
            (NormalB (AppE (VarE 'check)
                           (AppE (AppE (VarE logicName)
                                      (AppE (VarE 'unsafeFromBuiltinData) (VarE (mkName "param"))))
                                (AppE (VarE 'unsafeFromBuiltinData) (VarE (mkName "ctx"))))))
            []]

-- ============================================================================
-- 4. HELPERS
-- ============================================================================

-- | (Internal) Finds a validator singleton ('SomeValidator') from an 'SValidatorList' by its name ('Text').
findValidatorSingletonByName :: SValidatorList vs -> Text -> Maybe SomeValidator
findValidatorSingletonByName SVNil _ = Nothing
findValidatorSingletonByName (SVCons v rest) name =
  if getValidatorNameFromSingleton v == name
    then Just (SomeValidator v)
    else findValidatorSingletonByName rest name

-- | (Internal) Recursively builds a Template Haskell 'Type' representation from an 'SParamList'.
-- Handles empty lists -> (), single params -> Type, multiple params -> (Type1, (Type2, ...)).
buildParamThType :: SParamList ps -> Q Type
buildParamThType SPNil = [t| () |]
buildParamThType (SPCons _ tProxy SPNil) = pure $ typeRepToTH (typeRep tProxy)
buildParamThType (SPCons _ tProxy rest) = do
  let thisType = typeRepToTH (typeRep tProxy)
  restType <- buildParamThType rest
  pure $ AppT (AppT (TupleT 2) thisType) restType

-- | (Internal) Converts a 'TypeRep' (from Data.Typeable) into a Template Haskell 'Type'.
-- Handles type constructors and applications.
typeRepToTH :: TypeRep -> Type
typeRepToTH rep =
    let (tyCon, tyArgs) = splitTyConApp rep
        con = ConT (mkName (show tyCon))
    in foldl AppT con (map typeRepToTH tyArgs)