{-|
Module      : Modsefa.Core.Transaction.Predicate
Description : Evaluation of Modsefa predicates during transaction building.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module provides the 'evaluatePredicate' function, which takes a singleton
representation of a 'Modsefa.Core.Foundation.Types.TypedPredicate' ('SPredicate')
and evaluates it against a concrete datum value ('GYDatum') at runtime.

This is primarily used during transaction building (specifically in
'Modsefa.Core.Transaction.Operations.resolveStateRefSingleton') to filter UTxOs
when resolving state references like 'Modsefa.Core.Foundation.Types.TypedUniqueWhere'
or 'Modsefa.Core.Foundation.Types.TypedAnyWhere'. It involves resolving any
'Modsefa.Core.Foundation.Types.TypedValue's within the predicate (potentially using
action parameters) and comparing them against the corresponding fields extracted
from the datum.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Predicate evaluation for transaction building
module Modsefa.Core.Transaction.Predicate
  ( evaluatePredicate
  ) where

import Data.List (elemIndex)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text, pack)
import Data.Typeable (typeRep)
import GHC.Generics (Generic, Rep)
import GHC.TypeLits (natVal, symbolVal)

import GeniusYield.Types (GYDatum)
import PlutusLedgerApi.V3 (toBuiltinData)
import PlutusTx (fromBuiltinData)

import Modsefa.Core.Foundation
  ( GExtractField, GetStateData, SStateType, SomeFieldValue(..), StateRepresentable
  , TypedPredicate
  )
import Modsefa.Core.Singletons
  ( FromEnumValue(fromEnumValue), SParamTuple(..), SPredicate(..), STypedValue(..)
  , extractFieldFromDatum
  )


-- ============================================================================
-- 1. PREDICATE EVALUATION
-- ============================================================================

-- | Evaluates an 'SPredicate' against a given 'GYDatum' value at runtime.
-- It resolves field values from the datum and compares them against values
-- derived from the predicate specification (which might involve action parameters).
evaluatePredicate ::
  forall st (pred :: TypedPredicate st) params.
  ( StateRepresentable st
  , Generic (GetStateData st)
  , GExtractField (Rep (GetStateData st))
  ) =>
  SStateType st ->
  SPredicate pred ->
  SParamTuple params ->
  [Text] ->
  GYDatum ->
  Either Text Bool
evaluatePredicate stateType (SFieldEquals (fieldProxy :: Proxy field) (valueSpec :: STypedValue value)) params paramNames datum = do
  let fieldName = pack $ symbolVal fieldProxy
  fieldValue <- extractFieldValue stateType fieldName datum
  -- Resolve the expected value specified in the predicate.
  expectedValue <- resolveTypedValue valueSpec params paramNames
  return $ someFieldValueEquals fieldValue expectedValue
evaluatePredicate stateType (SAnd p1 p2) params paramNames datum = do
  -- Recursively evaluate the first sub-predicate.
  res1 <- evaluatePredicate stateType p1 params paramNames datum
  -- Recursively evaluate the second sub-predicate.
  res2 <- evaluatePredicate stateType p2 params paramNames datum
  -- Combine results using logical AND.
  return $ res1 && res2
-- Note: FieldGT and FieldLT predicates are not currently implemented here.


-- ============================================================================
-- 2. HELPER FUNCTIONS
-- ============================================================================

-- | (Internal) Compares two 'SomeFieldValue' existential wrappers for equality.
-- Uses 'toBuiltinData' for a safe comparison that works across different underlying types.
someFieldValueEquals :: SomeFieldValue -> SomeFieldValue -> Bool
someFieldValueEquals (SomeFieldValue v1) (SomeFieldValue v2) =
  toBuiltinData v1 == toBuiltinData v2

-- | (Internal) Extracts a field's value ('SomeFieldValue') from a 'GYDatum'.
-- Parses the datum into the specified 'StateType' and uses generic field extraction.
extractFieldValue ::
  forall st.
  ( StateRepresentable st
  , Generic (GetStateData st)
  , GExtractField (Rep (GetStateData st))
  ) =>
  SStateType st -> -- ^ Singleton specifying the expected 'StateType' of the datum.
  Text -> -- ^ Name of the field to extract.
  GYDatum -> -- ^ The datum value.
  Either Text SomeFieldValue -- ^ 'Right SomeFieldValue' or 'Left error'.
extractFieldValue _ fieldName datum = do
  -- Convert GYDatum to BuiltinData for parsing.
  let builtinData = toBuiltinData datum
  -- Attempt to parse BuiltinData into the Haskell record type.
  case fromBuiltinData builtinData :: Maybe (GetStateData st) of
    Nothing -> Left "Failed to parse datum into the specified StateType"
    Just stateData ->
      -- If parsing succeeds, extract the field using the generic helper.
      case extractFieldFromDatum @st fieldName stateData of
        Nothing -> Left $ "Field not found in datum: " <> fieldName
        Just value -> Right value

-- | (Internal) Resolves an 'STypedValue' singleton (representing a value source in the predicate)
-- into a concrete 'SomeFieldValue'. Handles 'SParamValue', 'SEnumValue', 'SIntValue'.
resolveTypedValue ::
  forall value params.
  STypedValue value -> -- ^ Singleton representing the value source.
  SParamTuple params -> -- ^ Action parameters tuple.
  [Text] -> -- ^ Action parameter names list.
  Either Text SomeFieldValue -- ^ Resolved value or error.
-- Case 1: Value comes from an action parameter.
resolveTypedValue (SParamValue (paramProxy :: Proxy param)) params paramNames = do
    let paramName = pack $ symbolVal paramProxy
    -- Extract the parameter value by name. Note: This helper returns SomeFieldValue directly.
    extractParamAsSomeFieldValue paramName paramNames params
-- Case 2: Value is an enum constructor.
resolveTypedValue (SEnumValue (_ :: Proxy t) (symbolProxy :: Proxy s)) _ _ =
  let enumSymbol = symbolVal symbolProxy
  in case fromEnumValue @t enumSymbol of
    Just (val :: t) -> Right $ SomeFieldValue val
    Nothing -> Left $ "Invalid enum value '" <> pack enumSymbol <> "' for type " <> pack (show (typeRep (Proxy @t)))
resolveTypedValue (SIntValue natProxy) _ _ =
  let intValue = natVal natProxy
  -- Wrap the Integer in SomeFieldValue.
  in Right $ SomeFieldValue (fromInteger intValue :: Integer)
resolveTypedValue (SStateFieldValue _ _) _ _ =
  Left "StateFieldValue is not supported within predicates for UTxO selection."
resolveTypedValue SCurrentTime _ _ =
  Left "CurrentTime is not supported within predicates for UTxO selection."
resolveTypedValue (SAddValue _ _) _ _ =
  Left "AddValue is not supported within predicates for UTxO selection."
resolveTypedValue (SSubtractValue _ _) _ _ =
  Left "SubtractValue is not supported within predicates for UTxO selection."
resolveTypedValue (SMultiplyValue _ _) _ _ =
  Left "MultiplyValue is not supported within predicates for UTxO selection."
resolveTypedValue (SDivideValue _ _) _ _ =
  Left "DivideValue is not supported within predicates for UTxO selection."

-- | (Internal) Helper to extract a parameter value from an 'SParamTuple' by name ('Text'),
-- returning it wrapped in 'SomeFieldValue' without casting.
extractParamAsSomeFieldValue :: Text -> [Text] -> SParamTuple ps -> Either Text SomeFieldValue
extractParamAsSomeFieldValue name names tuple =
  -- Find the index of the parameter name.
  case elemIndex name names of
    Nothing -> Left $ "Predicate failed: Action parameter '" <> name <> "' not found."
    Just i  -> go i tuple
  where
    -- Recursive helper to extract value at index 'n'.
    go :: Int -> SParamTuple ps' -> Either Text SomeFieldValue
    go 0 (STupleCons val _)    = Right (SomeFieldValue val)
    go n (STupleCons _ rest) = go (n - 1) rest
    go _ STupleNil           = Left "Parameter index out of bounds"