{-|
Module      : Modsefa.CodeGen.Generation.Validators
Description : TH splices for generating validator code and AppValidatorScripts instances.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC (requires TemplateHaskell)

This module provides the primary Template Haskell (TH) splices that generate
the Plutus Tx code and the necessary "glue" instances for a Modsefa application.

A typical setup involves two generated modules:

1.  A '...Generated.hs' module that uses `$(generateAppCode @MyApp)` to
    create all the raw Plutus Tx validator functions and their 'CompiledCode'
    representations (e.g., `feedValidatorCompiledCode`).

2.  A '...Scripts.hs' module that uses `$(generateAppValidatorScripts @MyApp)`
    to create the `instance AppValidatorScripts MyApp`. This instance implements
    the 'getValidatorScript' method, which acts as a dispatcher to connect a
    validator's type to its compiled code from '...Generated.hs'.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Modsefa.CodeGen.Generation.Validators
  ( -- * High-Level Application Splices
    generateAppCode
  , generateAppValidatorScripts
  ) where

import Data.Char (toLower)
import Data.List (concat)
import Data.Text (Text, unpack)
import Data.Typeable (TypeRep, Typeable, splitTyConApp, typeRep, type (:~:)(Refl), Proxy (Proxy))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Language.Haskell.TH
  ( Body(NormalB), Clause(Clause), Dec(FunD, PragmaD, SigD), Exp(AppE, VarE)
  , Inline(Inlinable), Name, Pat(VarP), Phases(AllPhases), Pragma(InlineP), Q
  , Quote (newName), RuleMatch(FunLike), Type(AppT, ArrowT, ConT, TupleT), caseE
  , clause, conT, funD, match, litT, mkName, normalB, sigD, strTyLit, varE, varP
  , varT
  )
import Prelude 
  ( Maybe(..), String, Traversable(..), fail, foldl, map, pure, return, show, ($)
  , (++), (==)
  )

import GeniusYield.Types (GYScript)
import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.V3
  ( BuiltinData, ScriptContext, ToData(toBuiltinData), unsafeFromBuiltinData
  )
import PlutusTx (CompiledCode, liftCode, unsafeApplyCode)
import PlutusTx.Plugin (plc)
import PlutusTx.Prelude (Bool, BuiltinUnit, check)

import Modsefa.Core.Foundation
  ( ActionTransitions, AppInstanceParameters, AppSpec, AppStates, InitialAppState
  , ParameterDerivations, Params, ParamsToValue, ValidatorAppName
  , ValidatorDef(Validator), ValidatorPlutusVersion, ValidatorSpec, Validators
  )
import Modsefa.Core.IR.Compiler (compileIR)
import Modsefa.Core.IR.Types (AppIR(..), StateInfoIR, ValidatorIR(..))
import Modsefa.Core.Singletons
  ( AutoSingletonActionTransitionList, AutoSingletonAppInstanceParams
  , AutoSingletonAppStateList, AutoSingletonInitialAppState
  , AutoSingletonParamDerivationList, AutoSingletonParamList
  , AutoSingletonValidators, SAppSpec(SAppSpec), SParamList(..), SValidator(..)
  , SValidatorList(..), SomeValidator(..), autoSingletonFull, autoSingletonParamList
  , getValidatorNameFromSingleton
  )
import Modsefa.Core.ValidatorScript (AppValidatorScripts(..))

import Modsefa.CodeGen.Generation.Logic (generateLogicFromIR)


-- ============================================================================
-- 1. HIGH-LEVEL APPLICATION SPLICES
-- ============================================================================

-- | Generates all core validator code for a given application 'app'.
-- |
-- | This is the primary code generation splice. When called, it generates:
-- | 1. The parameterized Plutus Tx logic function for each validator (e.g., `mkParameterizedFeedValidator`).
-- | 2. The Plutus Tx wrapper for each validator (e.g., `mkWrappedParameterizedFeedValidator`).
-- | 3. The `CompiledCode` function for each validator (e.g., `feedValidatorCompiledCode`).
-- |
-- | This splice should be called once in a dedicated module (e.g., 'Generated.hs').
-- |
-- | Usage: $(generateAppCode @FeedApp)
generateAppCode :: forall app.
  ( AppSpec app
  , Typeable app
  , KnownSymbol (InitialAppState app)
  , AutoSingletonValidators (Validators app)
  , AutoSingletonAppStateList (AppStates app)
  , AutoSingletonInitialAppState (InitialAppState app)
  , AutoSingletonActionTransitionList (ActionTransitions app)
  , AutoSingletonAppInstanceParams (AppInstanceParameters app)
  , AutoSingletonParamDerivationList (ParameterDerivations app)
  , GeneratePlcCompilers (Validators app)
  ) => Q [Dec]
generateAppCode = do
    -- 1. Call the first splice
    validatorDecs <- generateAllValidatorsForApp @app
    -- 2. Call the second splice
    compilerDecs <- generatePlcCompilers @app
    -- 3. Combine them
    return (validatorDecs ++ compilerDecs)

-- | Generates the 'AppValidatorScripts' instance for a given application 'app'.
-- |
-- | This splice generates the "glue" code that maps a validator's type
-- | (e.g., `Proxy @FeedValidator`) to its compiled code (generated by 'generateAppCode').
-- | It implements the 'getValidatorScript' method as a dispatcher.
-- |
-- | This splice should be called once in a dedicated module (e.g., 'Scripts.hs').
-- |
-- | Usage: $(generateAppValidatorScripts @FeedApp)
generateAppValidatorScripts :: forall app.
  ( AppSpec app
  , Typeable app
  , GenerateValidatorCases (Validators app)
  ) => Q [Dec]
generateAppValidatorScripts = do
  let appType = conT $ mkName $ show $ typeRep (Proxy @app)
  
  -- 1. Create a Name for the 'params' variable
  paramsName <- newName "params"
  
  -- 2. Generate the nested case expression body (Type: Q Exp)
  caseExp <- genValidatorCases @(Validators app) paramsName
  
  -- 3. Generate the instance using a declaration quote [d| ... |]
  --    This lets the GHC parser handle the 'forall v.' syntax correctly.
  [d|
    instance AppValidatorScripts $appType where
      getValidatorScript :: forall v. (ValidatorSpec v, Typeable v) =>
        Proxy v ->
        ParamsToValue (Params v) ->
        GYScript (ValidatorPlutusVersion v)
      getValidatorScript _ $(varP paramsName) = $(pure caseExp)
    |]

-- ============================================================================
-- 2. INTERNAL: APP CODE GENERATION (Helpers for generateAppCode)
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
  decs <- traverse (generateValidatorFromIR (appIRStates appIR) sValidators) (appIRValidators appIR)
  return (concat decs)

-- | (Internal) Helper class to generate the '...CompiledCode' definitions
class GeneratePlcCompilers (vs :: [ValidatorDef]) where
  genPlcCompilers :: Q [Dec]

-- | Base case: No validators, no code.
instance GeneratePlcCompilers '[] where
  genPlcCompilers = pure []

-- | Recursive case: Generate code for 'v_concrete' and recurse.
instance ( ValidatorSpec v_concrete, Typeable v_concrete
         , AutoSingletonParamList (Params v_concrete) -- We need this
         , GeneratePlcCompilers rest
         )
      => GeneratePlcCompilers ('Validator v_concrete ': rest) where
  
  genPlcCompilers = do
    -- 1. Predict the names
    let vAppName = symbolVal (Proxy @(ValidatorAppName v_concrete))
    let compiledCodeFunName = mkName (uncapitalize vAppName ++ "CompiledCode")
    let wrappedFunName = mkName ("mkWrappedParameterized" ++ vAppName)
    let vAppNameLit = litT (strTyLit vAppName)

    -- 2. Get the parameters
    let paramList = autoSingletonParamList @(Params v_concrete)
    
    -- 3. Generate Decs based on whether params are empty
    thisDecs <- case paramList of
      SPNil -> do
        -- Case: No params
        let sigType = [t| CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit) |]
        sig <- sigD compiledCodeFunName sigType
        let body = [| plc (Proxy @($vAppNameLit)) $(varE wrappedFunName) |]
        fun <- funD compiledCodeFunName [clause [] (normalB body) []]
        return [sig, fun]
      
      _ -> do
        -- Case: Has params
        let paramTypeQ = buildParamThType paramList
        let paramVarName = mkName "param"
        
        -- Splice 'paramTypeQ' directly into the signature quote
        let sigType = [t| $paramTypeQ -> CompiledCode (BuiltinData -> BuiltinUnit) |]
        sig <- sigD compiledCodeFunName sigType

        -- fun: ... param = plc (Proxy @"...") mkWrapped... `applyCode` liftCode ... (toBuiltinData param)
        let body = [|
              plc (Proxy @($vAppNameLit)) $(varE wrappedFunName)
                `PlutusTx.unsafeApplyCode` PlutusTx.liftCode PlutusCore.Version.plcVersion110 (PlutusLedgerApi.V3.toBuiltinData $(varE paramVarName))
              |]
        -- We must use 'paramTypeQ' to generate the 'varP' pattern
        fun <- funD compiledCodeFunName [clause [varP paramVarName] (normalB body) []]
        return [sig, fun]

    -- 4. Recurse
    restDecs <- genPlcCompilers @rest
    
    -- 5. Return all Decs
    return (thisDecs ++ restDecs)

-- | (Internal) Splice to generate all '...CompiledCode' boilerplate functions
generatePlcCompilers :: forall app.
  ( AppSpec app
  , Typeable app
  , GeneratePlcCompilers (Validators app)
  ) => Q [Dec]
generatePlcCompilers = genPlcCompilers @(Validators app)

-- ============================================================================
-- 3. INTERNAL: APPVALIDATORSCRIPTS GENERATION
-- ============================================================================

-- | (Internal) A type class to recursively build the case-expression for 'getValidatorScript'.
class GenerateValidatorCases (vs :: [ValidatorDef]) where
  -- | Generates the nested case expression.
  genValidatorCases :: Name -- ^ The 'Name' of the 'params' variable.
                    -> Q Exp  -- ^ The 'Q' computation returning the 'case' 'Exp'.

-- | Base case: No validators left, return the error.
instance GenerateValidatorCases '[] where
  genValidatorCases _ = [| error "No script implementation for this validator." |] -- Unchanged

-- | Recursive case: Generate a case for the head 'v_concrete' and recurse on 'rest'.
instance (ValidatorSpec v_concrete, Typeable v_concrete, GenerateValidatorCases rest)
      => GenerateValidatorCases ('Validator v_concrete ': rest) where
  
  genValidatorCases paramsName = do
    -- 1. Get the Type of the validator (e.g., FeedValidator)
    let vType = conT $ mkName $ show $ typeRep (Proxy @v_concrete)
    
    -- 2. Predict the name of the compiled code function by convention
    let vAppName = symbolVal (Proxy @(ValidatorAppName v_concrete))
    let compiledCodeFunName = mkName (uncapitalize vAppName ++ "CompiledCode")

    -- 3. Generate the 'rest' of the case expression (this is already a Q Exp)
    restOfCasesExp <- genValidatorCases @rest paramsName
    
    -- 4. Build this case arm's logic (this is also a Q Exp)
    let successBranchExp = [| scriptFromPlutus ($(varE compiledCodeFunName) $(varE paramsName)) |]
    
    -- 5. Build the full case expression using TH functions
    --    in the [d| ... |] quote in 'generateAppValidatorScripts' !!
    caseE [| eqT @($(varT (mkName "v"))) @($vType) |]
      [ match [p| Just Refl |] (normalB successBranchExp) []
      , match [p| Nothing   |] (normalB (pure restOfCasesExp)) []
      ]

-- ============================================================================
-- 4. INTERNAL: CORE VALIDATOR FUNCTION GENERATION
-- ============================================================================

-- | (Internal) Generates the full set of Haskell declarations ('Dec') for a single validator
generateValidatorFromIR :: [StateInfoIR] -- ^ The application state registry.
                        -> SValidatorList vs -- ^ Original validator singletons (needed for parameter types).
                        -> ValidatorIR -- ^ The IR for the specific validator to generate code for.
                        -> Q [Dec] -- ^ TH computation returning the generated declarations.
generateValidatorFromIR registry sValidators ir = do
  case findValidatorSingletonByName sValidators (validatorIRName ir) of
    Nothing -> fail $ "InternalCodeGenError: Could not find validator singleton for: " ++ unpack (validatorIRName ir)
    Just (SomeValidator (validator :: SValidator v)) -> do
      -- Generate the type signatures for the logic and wrapper functions.
      (logicSig, wrapperSig) <- generateValidatorTypeSignatures validator
      -- Generate the function definitions (bodies) and their INLINE pragmas.
      (logicFunc, wrapperFunc, logicPragma, wrapperPragma) <- generateFunctionsFromIR registry ir

      -- Return all generated declarations in the standard order.
      return [logicPragma, logicSig, logicFunc, wrapperPragma, wrapperSig, wrapperFunc]

-- | (Internal) Generates the Template Haskell 'Dec' values for the type signatures
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

-- | (Internal) Generates the Template Haskell 'Dec' values for the function definitions
generateFunctionsFromIR :: [StateInfoIR] -- ^ The application state registry.
                        -> ValidatorIR -- ^ The IR for the validator.
                        -> Q (Dec, Dec, Dec, Dec) -- ^ TH: (logicFuncDec, wrapperFuncDec, logicPragmaDec, wrapperPragmaDec).
generateFunctionsFromIR registry ir = do
  let vName = validatorIRName ir
  -- Construct the function names.
  let funcName = mkName ("mkParameterized" ++ unpack vName)
  let wrapperName = mkName ("mkWrappedParameterized" ++ unpack vName)

  -- Delegate the generation of the core validation logic (if/then/else based on redeemer).
  logicFunc <- generateLogicFromIR registry funcName ir

  -- Generate the standard Plutus Tx wrapper function.
  wrapperFunc <- generateWrapperFunction funcName wrapperName

  -- Generate INLINE pragmas, crucial for Plutus Tx optimization.
  let logicPragma = PragmaD (InlineP funcName Inlinable FunLike AllPhases)
  let wrapperPragma = PragmaD (InlineP wrapperName Inlinable FunLike AllPhases)

  return (logicFunc, wrapperFunc, logicPragma, wrapperPragma)

-- | (Internal) Generates the standard Plutus Tx wrapper function definition.
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
-- 5. INTERNAL: GENERAL HELPERS
-- ============================================================================

-- | (Internal) Helper to uncapitalize the first letter of a string.
uncapitalize :: String -> String
uncapitalize (c:cs) = toLower c : cs
uncapitalize []     = []

-- | (Internal) Finds a validator singleton ('SomeValidator') from an 'SValidatorList' by its name.
findValidatorSingletonByName :: SValidatorList vs -> Text -> Maybe SomeValidator
findValidatorSingletonByName SVNil _ = Nothing
findValidatorSingletonByName (SVCons v rest) name =
  if getValidatorNameFromSingleton v == name
    then Just (SomeValidator v)
    else findValidatorSingletonByName rest name

-- | (Internal) Recursively builds a Template Haskell 'Type' representation from an 'SParamList'.
buildParamThType :: SParamList ps -> Q Type
buildParamThType SPNil = [t| () |]
buildParamThType (SPCons _ tProxy SPNil) = pure $ typeRepToTH (typeRep tProxy)
buildParamThType (SPCons _ tProxy rest) = do
  let thisType = typeRepToTH (typeRep tProxy)
  restType <- buildParamThType rest
  pure $ AppT (AppT (TupleT 2) thisType) restType

-- | (Internal) Converts a 'TypeRep' (from Data.Typeable) into a Template Haskell 'Type'.
typeRepToTH :: TypeRep -> Type
typeRepToTH rep =
    let (tyCon, tyArgs) = splitTyConApp rep
        con = ConT (mkName (show tyCon))
    in foldl AppT con (map typeRepToTH tyArgs)