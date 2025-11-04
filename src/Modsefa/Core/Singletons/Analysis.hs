{-|
Module      : Modsefa.Core.Singletons.Analysis
Description : Analysis and query functions for Modsefa singleton types.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module provides functions for analyzing and querying singleton
specifications ('SAppSpec', 'SValidator', 'SActionSpec', etc.) at runtime.
These functions enable introspection of type-level specifications and are used
internally by code generation, transaction building, and validation systems to
extract necessary information like names, managed states, parameters, and relationships
between different parts of the specification.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Analysis and query functions for singleton types
--
-- This module provides functions for analyzing and querying singleton
-- specifications at runtime. These functions enable introspection of
-- type-level specifications and are used by code generation, transaction
-- building, and validation systems.
--
-- Organization:
--   1. Basic Extraction Functions
--   2. Validator Analysis  
--   3. Action Analysis
--   4. State Analysis
--   5. Parameter Analysis
--   6. Advanced Query Functions
--   7. Helper Functions
module Modsefa.Core.Singletons.Analysis
  ( -- * Basic Extraction Functions
    extractParamNames
  , getValidatorNameFromSingleton
  , extractStateName
  , getStateName
    
    -- * Validator Analysis
  , findValidatorManagingState
  , validatorManagesState
    
    -- * Action Analysis
  , findActionsForValidator
    
    -- * State Analysis
  , extractStateNamesFromStateList
  , extractStateFromRef
  , extractStateTypeFromRef'
  , stateInList
    
    -- * Parameter Analysis
  , findValidatorByName
  , getValidatorParams
  , extractParamByNameDirectWithProxy
    
    -- * Advanced Query Functions
  , analyzeValidatorComplete
    
    -- * Helper Functions
  , createAppInstance
  ) where

import Data.Kind (Type)
import Data.List (elemIndex)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text, pack)
import Data.Typeable (Typeable, (:~:)(Refl), cast, eqT)
import GHC.TypeLits (symbolVal)

import Modsefa.Core.Foundation
  ( AppSpec(AppInstanceParameters, Validators), ExtractOpsFromActionSteps
  , GetStateName, ParamsToValue, ResolveInstanceParamList, SStateType(..)
  , ValidatorSpec(Params, ValidatorAppName)
  )

import Modsefa.Core.Singletons.Types
  ( SActionSpec(..), SActionStep(SLet, SMap, SOp), SActionStepList(..)
  , SActionTransition(..), SActionTransitionList(..), SAppInstance(SAppInstance)
  , SAppSpec(..), SConstraint(..), SConstraintList(..)
  , SInstanceParams(SInstanceParams), SOperation(..), SOperationList(..)
  , SParamList(..), SParamTuple(..), SStateRef(..), SValidator(..)
  , SValidatorList(..), SStateList(..), SomeStateType(..), SomeValidator(..)
  )


-- ============================================================================
-- 1. BASIC EXTRACTION FUNCTIONS
-- ============================================================================

-- | Extracts the action name ('Text') from an 'SActionSpec'.
extractActionName :: SActionSpec app spec -> Text
extractActionName (SActionSpec nameProxy _ _ _) = pack $ symbolVal nameProxy

-- | Extracts a list of parameter names ('Text') from an 'SParamList'.
extractParamNames :: SParamList params -> [Text]  
extractParamNames SPNil = []
extractParamNames (SPCons nameProxy _ rest) = 
  pack (symbolVal nameProxy) : extractParamNames rest

-- | Extracts the validator's application name ('Text') from an 'SValidator'.
-- Uses the 'ValidatorAppName' type family via a 'KnownSymbol' constraint.
getValidatorNameFromSingleton :: forall v. ValidatorSpec v => SValidator v -> Text  
getValidatorNameFromSingleton _ = pack $ symbolVal (Proxy @(ValidatorAppName v))

-- | Extracts the state's symbolic name ('Text') from an 'SStateType'.
-- Uses the 'GetStateName' type family via a 'KnownSymbol' constraint.
extractStateName :: SStateType st -> Text
extractStateName (SStateType :: SStateType st) = 
  pack $ symbolVal (Proxy @(GetStateName st))

-- | Alias for 'extractStateName'.
getStateName :: SStateType st -> Text  
getStateName = extractStateName

-- ============================================================================
-- 2. VALIDATOR ANALYSIS
-- ============================================================================

-- | Gets a list of managed state names ('Text') for a validator identified by its name.
getManagedStatesByValidatorName :: SValidatorList validators -> Text -> [Text]
getManagedStatesByValidatorName SVNil _ = []
getManagedStatesByValidatorName (SVCons validator rest) targetName = 
  if getValidatorNameFromSingleton validator == targetName
  then getManagedStatesFromValidator validator
  else getManagedStatesByValidatorName rest targetName

-- | Extracts the names ('Text') of states managed by a specific 'SValidator'.
getManagedStatesFromValidator :: forall v. ValidatorSpec v => SValidator v -> [Text]
getManagedStatesFromValidator (SValidator _ managedStates _ _) = 
  extractStateNamesFromStateList managedStates

-- | Finds the 'SomeValidator' responsible for managing a given 'SomeStateType' within an 'SAppSpec'.
-- Returns 'Left' error message if no managing validator is found.
findValidatorManagingState :: forall app.
  SAppSpec app -> SomeStateType -> Either Text SomeValidator
findValidatorManagingState (SAppSpec validators _ _ _ _ _) = 
  searchValidatorList validators

-- | Helper for 'findValidatorManagingState': Recursively searches an 'SValidatorList'.
searchValidatorList :: SValidatorList validators -> SomeStateType -> Either Text SomeValidator
searchValidatorList SVNil _ = Left "No validator found managing this state"
searchValidatorList (SVCons validator rest) targetState = 
  if validatorManagesState validator targetState
    then Right (SomeValidator validator)
    else searchValidatorList rest targetState

-- | Checks if a given 'SValidator' manages the target 'SomeStateType'.
validatorManagesState :: SValidator v -> SomeStateType -> Bool
validatorManagesState (SValidator _ managedStates _ _) targetState = 
  stateInList targetState managedStates

-- | Finds a validator singleton ('SomeValidator') within an 'SValidatorList' by its name ('Text').
-- Returns 'Nothing' if not found.
findValidatorByName :: SValidatorList validators -> Text -> Maybe SomeValidator
findValidatorByName SVNil _ = Nothing
findValidatorByName (SVCons validator rest) targetName = 
  let validatorName = getValidatorNameFromSingleton validator
  in if validatorName == targetName
     then Just $ SomeValidator validator
     else findValidatorByName rest targetName

-- ============================================================================
-- 3. ACTION ANALYSIS
-- ============================================================================

-- | Wrapper to hold an 'SActionSpec' while preserving the application type parameter @app@.
-- Useful for returning action specs from analysis functions without resorting to existentials immediately.
data ActionSpecWithApp (app :: Type) where
  ActionSpecWithApp :: SActionSpec app spec -> ActionSpecWithApp app

-- | Finds all actions ('ActionSpecWithApp') within an 'SAppSpec' that have a
-- 'MustSpendValidatorParam' constraint matching the given validator name ('Text').
-- Returns a list of tuples containing the action and the name of the parameter being spent.
findActionsWithValidatorParam :: forall app. SAppSpec app -> Text -> [(ActionSpecWithApp app, Text)]
findActionsWithValidatorParam appSpec' validatorName = 
  let allActions = extractAllActionsFromTransitions appSpec'
  in [(action, paramName) | action <- allActions, 
                           Just paramName <- [extractValidatorParamName validatorName action]]

-- | Finds all actions ('ActionSpecWithApp') within an 'SAppSpec' that operate on
-- any of the states managed by the validator identified by 'validatorName'.
findActionsForValidator :: forall app.
  SAppSpec app -> Text -> [ActionSpecWithApp app]
findActionsForValidator appSpec'@(SAppSpec validators _ _ _ _ _) validatorName = 
  let allActions = extractAllActionsFromTransitions appSpec'
      managedStates = getManagedStatesByValidatorName validators validatorName
  in filter (actionOperatesOnStates managedStates) allActions

-- | Extracts all 'ActionSpecWithApp's from the 'ActionTransitions' list of an 'SAppSpec'.
extractAllActionsFromTransitions :: forall app. SAppSpec app -> [ActionSpecWithApp app]
extractAllActionsFromTransitions appSpec' = 
  case appSpec' of
    SAppSpec _ _ _ transitions _ _ -> extractActionsFromTransitionListHelper transitions

-- | Helper for 'extractAllActionsFromTransitions': Recursively traverses the 'SActionTransitionList'.
extractActionsFromTransitionListHelper :: SActionTransitionList app transitions -> [ActionSpecWithApp app]
extractActionsFromTransitionListHelper SATLNil = []
extractActionsFromTransitionListHelper (SATLCons transition rest) = 
  extractActionFromTransitionHelper transition : extractActionsFromTransitionListHelper rest

-- | Helper for 'extractActionsFromTransitions': Extracts the 'SActionSpec' from a single 'SActionTransition'.
extractActionFromTransitionHelper :: forall app transition.
  SActionTransition app transition -> ActionSpecWithApp app
extractActionFromTransitionHelper (SActionTransition actionSpec _ _) = ActionSpecWithApp actionSpec

-- | Gets a list of state names ('Text') involved in the operations of an action ('ActionSpecWithApp').
getActionOperatedStates :: ActionSpecWithApp app -> [Text]
getActionOperatedStates (ActionSpecWithApp actionSpec) = 
  case actionSpec of
    SActionSpec _ steps _ _ -> extractStatesFromOperationList (extractOpsFromActionStepList steps)

-- | If an action ('ActionSpecWithApp') contains a 'MustSpendValidatorParam' constraint
-- matching 'validatorName', returns 'Just' the name ('Text') of the parameter being spent.
extractValidatorParamName :: Text -> ActionSpecWithApp app -> Maybe Text
extractValidatorParamName validatorName (ActionSpecWithApp actionSpec) = 
  case actionSpec of
    SActionSpec _ _ constraints _ -> findInConstraintList constraints
  where
    -- Recursively checks the constraint list
    findInConstraintList :: SConstraintList constraints -> Maybe Text
    findInConstraintList SCLNil = Nothing
    findInConstraintList (SCLCons constraint rest) = 
      case findInConstraint constraint of
        Just paramName -> Just paramName
        Nothing -> findInConstraintList rest
    
    -- Checks a single constraint
    findInConstraint :: SConstraint constraint -> Maybe Text
    findInConstraint constraint = case constraint of
      SMustSpendValidatorParam vNameProxy paramNameProxy -> 
        if pack (symbolVal vNameProxy) == validatorName
        then Just (pack $ symbolVal paramNameProxy)
        else Nothing
      _ -> Nothing -- Only interested in MustSpendValidatorParam

-- | Extracts the action name ('Text') from an 'ActionSpecWithApp'.
extractActionNameLocal :: ActionSpecWithApp app -> Text
extractActionNameLocal (ActionSpecWithApp actionSpec) = extractActionName actionSpec

-- | Extracts a textual representation of all constraints in an action ('ActionSpecWithApp').
-- Primarily for debugging/analysis.
extractAllConstraints :: ActionSpecWithApp app -> [Text]
extractAllConstraints (ActionSpecWithApp actionSpec) = 
  case actionSpec of
    SActionSpec _ _ constraints _ -> extractConstraintNames constraints
  where
    extractConstraintNames :: SConstraintList constraints -> [Text]
    extractConstraintNames SCLNil = []
    extractConstraintNames (SCLCons constraint rest) = 
      getConstraintName constraint : extractConstraintNames rest
    
    getConstraintName :: SConstraint constraint -> Text
    getConstraintName constraint = case constraint of
      SMustSpendValidatorParam vNameProxy paramNameProxy -> 
        "MustSpendValidatorParam(" <> pack (symbolVal vNameProxy) <> ", " <> pack (symbolVal paramNameProxy) <> ")"
      SMustSpendActionParam paramNameProxy ->
        "MustSpendActionParam(" <> pack (symbolVal paramNameProxy) <> ")"
      SMustBeSignedByState _ fieldProxy ->
        "MustBeSignedByState(..., " <> pack (symbolVal fieldProxy) <> ")"
      SMustNotExist _ ->
        "MustNotExist(...)"
      SMustBeSignedByParam paramProxy ->
        "MustBeSignedByParam(" <> pack (symbolVal paramProxy) <> ")"
      SMustAddToAggregateState stype _ ->
        "MustAddToAggregateState(" <> extractStateName stype <> ", ...)"
      SMustWithdrawFromAggregateState stype _ _ ->
        "MustWithdrawFromAggregateState(" <> extractStateName stype <> ", ...)"
      -- Add other constraint types here if needed for analysis output

-- ============================================================================
-- 4. STATE ANALYSIS
-- ============================================================================

-- | Extracts a list of state names ('Text') from an 'SStateList'.
extractStateNamesFromStateList :: SStateList states -> [Text]
extractStateNamesFromStateList SSNil = []
extractStateNamesFromStateList (SSCons stateType rest) = 
  extractStateName stateType : extractStateNamesFromStateList rest

-- | Extracts the 'SOperationList' from an 'SActionStepList'.
extractOpsFromActionStepList :: SActionStepList steps -> SOperationList (ExtractOpsFromActionSteps steps)
extractOpsFromActionStepList ASSLNil = SOLNil
extractOpsFromActionStepList (ASSLCons (SOp op) rest) = SOLCons op (extractOpsFromActionStepList rest)
extractOpsFromActionStepList (ASSLCons (SLet _ op) rest) = SOLCons op (extractOpsFromActionStepList rest)
extractOpsFromActionStepList (ASSLCons (SMap op _ _) rest) = SOLCons op (extractOpsFromActionStepList rest)

-- | Extracts a list of state names ('Text') involved in an 'SOperationList'.
extractStatesFromOperationList :: SOperationList ops -> [Text]
extractStatesFromOperationList SOLNil = []
extractStatesFromOperationList (SOLCons op rest) = 
  extractStateFromOp op : extractStatesFromOperationList rest

-- | Extracts the primary state name ('Text') associated with a single 'SOperation'.
extractStateFromOp :: SOperation op -> Text
extractStateFromOp op = case op of
  SCreate stateTypeProxy _ _ -> extractStateName stateTypeProxy
  SUpdate stateRef _ _ -> extractStateFromRef stateRef
  SDelete stateRef _ -> extractStateFromRef stateRef
  SReference stateRef _ -> extractStateFromRef stateRef

-- | Extracts the state name ('Text') from an 'SStateRef'.
extractStateFromRef :: SStateRef st ref -> Text
extractStateFromRef (STypedTheOnlyInstance stateType) = extractStateName stateType
extractStateFromRef (STypedUniqueWhere stateType _) = extractStateName stateType  
extractStateFromRef (STypedAny stateType) = extractStateName stateType
extractStateFromRef (STypedAnyWhere stateType _) = extractStateName stateType
extractStateFromRef (STypedByLabel stateType _) = extractStateName stateType

-- | Extracts the 'SStateType' singleton from an 'SStateRef'
extractStateTypeFromRef' :: SStateRef st ref -> SStateType st
extractStateTypeFromRef' (STypedTheOnlyInstance st) = st
extractStateTypeFromRef' (STypedUniqueWhere st _)  = st
extractStateTypeFromRef' (STypedAny st) = st
extractStateTypeFromRef' (STypedAnyWhere st _) = st
extractStateTypeFromRef' (STypedByLabel st _) = st

-- | Checks if a 'SomeStateType' exists within an 'SStateList'.
stateInList :: SomeStateType -> SStateList states -> Bool
stateInList _ SSNil = False
stateInList (SomeStateType targetState) (SSCons state rest) =
  statesEqual targetState state || stateInList (SomeStateType targetState) rest

-- | Compares a 'SomeStateType' existential wrapper with an 'SStateType' singleton for equality.
statesEqual :: forall st1 st2. SStateType st1 -> SStateType st2 -> Bool
statesEqual (SStateType :: SStateType st1) (SStateType :: SStateType st2) =
  case eqT @st1 @st2 of -- Use safe type equality check
    Just Refl -> True   -- If types are equal, they represent the same state
    Nothing -> False

-- | Checks if an action ('ActionSpecWithApp') operates on any state whose name is in the provided list ('[Text]').
actionOperatesOnStates :: [Text] -> ActionSpecWithApp app -> Bool
actionOperatesOnStates managedStates action = 
  let operatedStates = getActionOperatedStates action
  in any (`elem` managedStates) operatedStates

-- ============================================================================
-- 5. PARAMETER ANALYSIS  
-- ============================================================================

-- | Extracts the 'SParamList' (parameter definitions) from an 'SValidator'.
getValidatorParams :: SValidator v -> SParamList (Params v)
getValidatorParams (SValidator params _ _ _) = params

-- | Extracts a parameter value of type @t@ directly from an 'SParamTuple' by name ('Text').
-- Uses a 'Proxy t' to specify the expected type and performs a safe 'cast'.
-- Requires the list of parameter names ('[Text]') to find the correct index.
extractParamByNameDirectWithProxy :: forall t params. 
  Typeable t =>
  Proxy t -> Text -> [Text] -> SParamTuple params -> Either Text t
extractParamByNameDirectWithProxy _ targetName paramNames params = do
  -- Find the index of the parameter name
  position <- case elemIndex targetName paramNames of
    Just pos -> Right pos
    Nothing -> Left $ "Parameter not found: " <> targetName
  -- Extract the value at that index and try to cast it
  extractParamAtIndexDirect position params
  where
    -- Recursive helper to get the value at a specific index
    extractParamAtIndexDirect :: forall params'. Int -> SParamTuple params' -> Either Text t
    extractParamAtIndexDirect n (STupleCons val rest)
      | n == 0 = case cast val of
          Just typedVal -> Right typedVal
          Nothing -> Left "Parameter has wrong type"
      | otherwise = extractParamAtIndexDirect (n - 1) rest
    extractParamAtIndexDirect _ STupleNil = Left "Parameter index out of bounds"

-- ============================================================================
-- 6. ADVANCED QUERY FUNCTIONS
-- ============================================================================

-- | Prints a comprehensive analysis of a specific validator within an 'SAppSpec' to IO.
-- Shows initialization actions (spending validator param) and operational actions (affecting managed states).
-- Useful for debugging and understanding validator interactions.
analyzeValidatorComplete :: SAppSpec app -> Text -> IO ()
analyzeValidatorComplete appSpec' validatorName = do
  putStrLn $ "=== Complete Analysis for validator: " ++ show validatorName ++ " ==="
  
  -- Find actions spending this validator's instance parameter (often initialization)
  let initActions = findActionsWithValidatorParam appSpec' validatorName
  putStrLn $ "\nInitialization actions (spend validator parameter): " ++ show (length initActions)
  mapM_ printActionInfo initActions
  
  -- Find all actions involving states managed by this validator
  let allActions = findActionsForValidator appSpec' validatorName
  putStrLn $ "\nAll actions affecting this validator: " ++ show (length allActions)
  mapM_ printActionInfoDetailed allActions
  
  -- Filter out init actions to show only operational actions
  let operationActions = filter (\action -> not $ actionInList action (map fst initActions)) allActions
  putStrLn $ "\nOperation actions (don't spend validator parameter): " ++ show (length operationActions)
  mapM_ printActionInfoDetailed operationActions
  
  where
    -- Helper to print info for init actions
    printActionInfo :: (ActionSpecWithApp app, Text) -> IO ()
    printActionInfo (action, paramName) = do
      let actionName = extractActionNameLocal action
      let operatedStates = getActionOperatedStates action
      putStrLn $ "  - " ++ show actionName ++ " (param: " ++ show paramName ++ ", states: " ++ show operatedStates ++ ")"
    
    -- Helper to print detailed info for any action affecting the validator
    printActionInfoDetailed :: ActionSpecWithApp app -> IO ()
    printActionInfoDetailed action = do
      let actionName = extractActionNameLocal action
      let operatedStates = getActionOperatedStates action
      let constraints = extractAllConstraints action
      let hasValidatorParam = case extractValidatorParamName validatorName action of
            Just param -> " (spends " ++ show param ++ ")"
            Nothing -> " (no validator param)"
      putStrLn $ "  - " ++ show actionName ++ hasValidatorParam ++ 
                 " (states: " ++ show operatedStates ++ ", constraints: " ++ show (length constraints) ++ ")"
    
    -- Helper to check if an action (by name) is in a list of actions
    actionInList :: ActionSpecWithApp app -> [ActionSpecWithApp app] -> Bool
    actionInList target actions = extractActionNameLocal target `elem` map extractActionNameLocal actions

-- ============================================================================
-- 7. HELPER FUNCTIONS
-- ============================================================================

-- | Constructs an 'SAppInstance' runtime value by combining the application's
-- singleton specification ('SAppSpec') with its resolved instance parameters.
-- Requires necessary 'Typeable' and 'Show' constraints on the resolved parameter type.
createAppInstance :: 
  ( Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
  , Show (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
  ) =>
  SAppSpec app -> -- ^ The singleton specification for the application.
  ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)) -> -- ^ The resolved value-level instance parameters.
  SAppInstance app -- ^ The resulting application instance value.
createAppInstance spec params = SAppInstance spec (SInstanceParams params)
