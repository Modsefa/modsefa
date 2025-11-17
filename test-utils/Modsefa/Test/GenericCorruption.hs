{-|
Module      : Modsefa.Test.GenericCorruption
Description : Utilities for generically corrupting datum fields for testing.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC (requires TypeApplications, GHC.Generics, TypeOperators, etc.)

This module provides the 'GCorruptField' type class and the 'corruptFieldGenerically'
function. These allow targeting a specific field within a state datum record (identified
by its field name as a 'String') and attempting to replace its value with new 'Data',
leveraging GHC.Generics to traverse the record structure.

This is primarily intended for negative testing of Modsefa validator scripts, allowing
developers to simulate invalid state transitions by corrupting datums before building
a transaction.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Modsefa.Test.GenericCorruption 
  ( GCorruptField
  , corruptFieldGenerically
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text, pack, unpack)
import Data.Typeable (Typeable)
import Debug.Trace (trace)
import GHC.Generics (Generic(..), K1(K1), M1(..), S, U1(..), (:*:)(..))
import GHC.TypeLits (KnownSymbol, symbolVal)

import PlutusLedgerApi.V3 (Data, FromData, ToData, fromData)

import Modsefa.Core.Foundation (MetaSelName, StateDatum, StateSpec)


-- ============================================================================
-- * Generic Corruption Type Class
-- ============================================================================

-- | Generic type class defining the corruption logic based on the 'Rep'resentation of a type.
-- Instances traverse the generic structure ('M1' for metadata, ':*:' for products)
-- until they reach the target field ('M1 S sel (K1 i a)') where the corruption is attempted.
class GCorruptField (rep :: Type -> Type) where
  -- | The core generic corruption function.
  gCorruptField ::
    String ->  -- ^ Target field name (as a String).
    Data ->    -- ^ The Plutus 'Data' to replace the field's value with.
    rep p ->   -- ^ The original generic representation of the datum.
    Either Text (rep p)  -- ^ 'Right' the (potentially) corrupted representation, or 'Left' error.

-- | Instance for 'M1' (metadata wrappers like constructors, selectors, datatypes).
-- Simply recurses into the inner representation 'f'. Covers D1, C1, S1 etc.
instance {-# OVERLAPPABLE #-} GCorruptField f => GCorruptField (M1 i c f) where
  gCorruptField fieldName corruptionValue (M1 x) = 
    M1 <$> gCorruptField fieldName corruptionValue x

-- | Instance for ':*:' (product types, representing multiple fields in a record).
-- Attempts corruption on the left ('a') and right ('b') sides.
-- If corruption succeeds on one side, it keeps that result and the original value from the other side.
-- If corruption fails (e.g., type mismatch) on one side but succeeds on the other, it still returns the partially corrupted result.
-- If both fail independently (e.g., target field name not found in either subtree), returns the original.
instance {-# OVERLAPPING #-} (GCorruptField a, GCorruptField b) => GCorruptField (a :*: b) where
  gCorruptField fieldName corruptionValue (a :*: b) = 
    trace "🔄 Generic: Processing product type (a :*: b)" $
    case gCorruptField fieldName corruptionValue a of
      Right corruptedA -> 
        trace "✅ Generic: Left side processed, trying right side" $
        case gCorruptField fieldName corruptionValue b of
          Right corruptedB -> 
            trace "✅ Generic: Both sides processed" $
            Right (corruptedA :*: corruptedB)
          Left err -> 
            trace ("❌ Generic: Right side failed: " ++ unpack err) $
            Right (corruptedA :*: b)
      Left err -> 
        trace ("❌ Generic: Left side failed: " ++ unpack err ++ ", trying right side") $
        case gCorruptField fieldName corruptionValue b of
          Right corruptedB -> Right (a :*: corruptedB)
          Left rightErr -> Left $ err <> " | " <> rightErr

-- | Instance for 'K1' wrapped in 'M1 S sel' (representing a single record field with selector metadata).
-- This is the core logic where the field name is checked and corruption is attempted.
-- Requires 'KnownSymbol' for the field name ('MetaSelName'), 'Typeable' for the field type 'a',
-- and Plutus 'FromData'/'ToData' constraints for decoding the 'corruptionValue' and for type 'a'.
instance {-# OVERLAPPING #-} forall i sel a.
  ( KnownSymbol (MetaSelName sel)
  , Typeable a
  , FromData a
  , ToData a
  ) => GCorruptField (M1 S sel (K1 i a)) where
  gCorruptField targetFieldName corruptionValue orig@(M1 (K1 _originalValue)) =
    -- Extract the field name from the selector metadata type 'sel'.
    let fieldName = symbolVal (Proxy @(MetaSelName sel))
    in
    trace ("🔍 Generic: Checking field '" ++ fieldName ++ "' against target '" ++ targetFieldName ++ "'") $
    if fieldName == targetFieldName
      then 
        trace ("✅ Generic: Found matching field '" ++ fieldName ++ "'") $
        case fromData corruptionValue :: Maybe a of
          Just newValue -> 
            trace "✅ Generic: Successfully decoded and corrupted" $
            Right $ M1 (K1 newValue)
          Nothing -> 
            trace "❌ Generic: Failed to decode corruption value" $
            Left $ pack $ "Failed to decode corruption value for field " ++ fieldName
      else
        trace ("⏭️  Generic: Skipping field '" ++ fieldName ++ "'") $
        Right orig

-- | Instance for 'U1' (unit constructor, representing constructors with no fields).
-- Corruption doesn't apply here, so it always succeeds returning the original 'U1'.
instance GCorruptField U1 where
  gCorruptField _ _ U1 = Right U1

-- ============================================================================
-- * Public API
-- ============================================================================

-- | Generic helper to corrupt a specific field within a state datum.
-- Uses 'Generic' to traverse the datum structure and apply a corruption function
-- to the field matching the given name.
corruptFieldGenerically ::
  forall s.
  ( StateSpec s
  , Generic (StateDatum s)
  , GCorruptField (Rep (StateDatum s))
  ) =>
  StateDatum s -> -- ^ The original state datum value (e.g., a 'ServiceConfig').
  String -> -- ^ The name of the field to corrupt (must match the record field name exactly).
  Data -> -- ^ The Plutus 'Data' to use as the corrupting value.
  Either Text (StateDatum s) -- ^ 'Right' the modified state datum, or 'Left' error message.
corruptFieldGenerically originalState fieldName corruptionValue = do
  -- Convert the original datum to its generic representation.
  let genericRep = from originalState
  -- Call the generic corruption function on the representation.
  corruptedRep <- gCorruptField fieldName corruptionValue genericRep
  -- Convert the (potentially) modified representation back to the original data type.
  return $ to corruptedRep