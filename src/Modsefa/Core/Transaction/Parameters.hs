{-|
Module      : Modsefa.Core.Transaction.Parameters
Description : Parameter resolution and extraction for Modsefa transaction building.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module provides utilities for resolving and extracting parameter values
required during the construction of transaction skeletons ('GYTxSkeleton').
It handles different sources of parameters:

1.  **Action Parameters:** Provided by the user when initiating an action (via 'SParamTuple').
2.  **Instance Parameters:** Defined in the 'AppSpec' ('AppInstanceParameters') and resolved at runtime for a specific application instance ('SInstanceParams').
3.  **Derived Parameters:** Calculated based on 'ParamDerivation' rules defined in the 'AppSpec', resolved into 'SDerivedParams'.

The core function 'buildParamsToValue' orchestrates the resolution, prioritizing derived parameters when available.
-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Parameter handling for transaction building
--
-- This module provides utilities for resolving and extracting parameters
-- needed during transaction building. It handles both action parameters
-- (passed by users) and instance parameters (derived from app configuration).
--
-- Organization:
--   1. Parameter Extraction Types 
--   2. Instance Parameter Extraction
--   3. Parameter Building
--   4. Derived Parameters
--   5. Batch Operation Support
module Modsefa.Core.Transaction.Parameters
  ( -- * Parameter Extraction Types
    Mappable(..)
    
    -- * Parameter Building
  , buildParamsToValue
    
    -- * Derived Parameters
  , extractFromDerivedParams
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Text (Text, pack)
import Data.Typeable (TypeRep, Typeable, cast, typeOf, typeRep)
import GHC.Generics 
  (C, D, Generic(..), K1(K1), M1(..), Meta(..), Rec0, S, (:*:)(..)
  )
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Unsafe.Coerce (unsafeCoerce)

import PlutusLedgerApi.V3 (ToData)

import Modsefa.Core.Foundation
  ( AppSpec(AppInstanceParameters, Validators), ParamsToValue
  , ResolveInstanceParamList
  )
import Modsefa.Core.Singletons
  ( SInstanceParams(..), SParamList(..), SParamTuple(..)
  , extractParamByNameDirectWithProxy
  )
  
import Modsefa.Core.Transaction.Types (SDerivedParams(..), SomeDerivedParams(..))


-- ============================================================================
-- 1. PARAMETER EXTRACTION TYPES
-- ============================================================================

-- | Runtime wrapper for a value of an unknown type, preserving 'Typeable'.
-- Used internally, e.g., when extracting instance parameters before casting.
data SomeTypedValue where
  SomeTypedValue :: (Typeable a) => a -> SomeTypedValue

-- | Show instance for 'SomeTypedValue', showing the type but not the value.
instance Show SomeTypedValue where
  show (SomeTypedValue value) = "SomeTypedValue(" ++ show (typeOf value) ++ ")"

-- ============================================================================
-- 2. INSTANCE PARAMETER EXTRACTION 
-- ============================================================================

-- | (Internal) Attempts to extract a value from 'SInstanceParams' if its type
-- matches the expected 'TypeRep'. Used as a fallback when a parameter isn't
-- found in action parameters or derived parameters.
tryExtractFromInstanceParams ::
  forall app.
  ( Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
  ) =>
  TypeRep -> -- ^ The expected type representation of the parameter.
  Text -> -- ^ The parameter name (used for error messages).
  SInstanceParams app -> -- ^ The resolved instance parameters.
  Either Text SomeTypedValue -- ^ 'Right SomeTypedValue' on type match, 'Left error' otherwise.
tryExtractFromInstanceParams expectedType _paramName (SInstanceParams instanceParamValues) =
  let actualType = typeOf instanceParamValues
  in if actualType == expectedType
     then Right (SomeTypedValue instanceParamValues)
     else Left $ "Instance parameter type mismatch. Expected: " <> pack (show expectedType) <> ", got: " <> pack (show actualType)

-- ============================================================================
-- 3. PARAMETER BUILDING
-- ============================================================================

-- | Constructs the value-level parameter tuple ('ParamsToValue') required by a validator script.
-- It resolves each parameter specified in the validator's 'SParamList' by searching:
-- 1. Derived parameters ('SDerivedParams'), if provided ('Just').
-- 2. Action parameters ('SParamTuple'), if not found in derived parameters or if derived parameters are 'Nothing'.
-- 3. Instance parameters ('SInstanceParams'), as a final fallback.
-- Type mismatches or missing parameters result in a 'Left' error.
buildParamsToValue ::
  forall validatorParams actionParams app.
  ( Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
  ) =>
  SParamList validatorParams -> -- ^ Singleton representing the validator's required parameters ('Params v').
  SParamTuple actionParams -> -- ^ Value-level tuple of action parameters provided by the user.
  [Text] -> -- ^ List of action parameter names (for indexed access).
  SInstanceParams app -> -- ^ Resolved application instance parameters.
  Maybe SomeDerivedParams -> -- ^ 'Just' the resolved derived parameters, or 'Nothing'.
  Either Text (ParamsToValue validatorParams) -- ^ 'Right' the resolved validator parameter value (often a tuple), or 'Left error'.
-- Base case: No parameters required, return unit.
buildParamsToValue SPNil _ _ _ _ = Right ()

-- Single parameter case: Resolve the single parameter.
buildParamsToValue (SPCons nameProxy typeProxy SPNil) paramTuple actionParamNames instanceParams' mDerivedParams = do
  let paramName = pack $ symbolVal nameProxy
  case mDerivedParams of
    -- If derived params exist, only look there.
    Just (SomeDerivedParams derivedParams) ->
      extractFromDerivedParams paramName derivedParams

    -- If no derived params, try action params then instance params.
    Nothing ->
      extractParamFromBothSources typeProxy paramName actionParamNames paramTuple instanceParams'

-- Multi-parameter case (recursive):
buildParamsToValue (SPCons nameProxy typeProxy rest) paramTuple actionParamNames instanceParams' mDerivedParams = do
  -- Recursively resolve the head parameter (using the single param case logic).
  -- Pass mDerivedParams down, as it applies to all parameters potentially.
  firstParam <- buildParamsToValue (SPCons nameProxy typeProxy SPNil) paramTuple actionParamNames instanceParams' mDerivedParams
  -- Recursively resolve the rest of the parameters.
  restParams <- buildParamsToValue rest paramTuple actionParamNames instanceParams' mDerivedParams

  -- Combine the head and tail results into a tuple. UnsafeCoerce is used
  -- because GHC cannot easily prove the resulting tuple type matches ParamsToValue.
  -- This is safe assuming ParamsToValue definition correctly matches this tuple structure.
  let tuple = (firstParam, restParams)
  return (unsafeCoerce tuple)

-- | (Internal) Helper for 'buildParamsToValue'. Attempts to extract a parameter first
-- from action parameters, falling back to instance parameters if not found.
extractParamFromBothSources ::
  forall a actionParams app.
  ( Typeable a
  , Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
  ) =>
  Proxy a -> -- ^ Proxy for the expected type.
  Text -> -- ^ Parameter name to find.
  [Text] -> -- ^ List of action parameter names.
  SParamTuple actionParams -> -- ^ Action parameter values.
  SInstanceParams app -> -- ^ Instance parameter values.
  Either Text a -- ^ Resulting value or error.
extractParamFromBothSources typeProxy paramName actionParamNames actionParams instanceParams' =
  -- Try extracting from action parameters first.
  case extractParamByNameDirectWithProxy typeProxy paramName actionParamNames actionParams of
    Right value -> Right value
    Left _ -> 
      case tryExtractFromInstanceParams (typeRep typeProxy) paramName instanceParams' of
        Right (SomeTypedValue val) -> 
          case cast val of
            Just typedVal -> Right typedVal
            Nothing -> Left $ "Instance parameter type mismatch for: " <> paramName
        Left err -> Left $ "Parameter not found: " <> paramName <> " (" <> err <> ")"

-- ============================================================================
-- 4. DERIVED PARAMETERS
-- ============================================================================

-- | Extracts and casts a parameter value of expected type 'a' from resolved 'SDerivedParams' by name.
extractFromDerivedParams ::
  forall a params.
  Typeable a => -- ^ Expected type 'a' of the parameter.
  Text -> -- ^ Name of the parameter to extract.
  SDerivedParams params -> -- ^ The resolved derived parameters singleton.
  Either Text a -- ^ 'Right' the value if found and type matches, 'Left error' otherwise.
-- Base case: Parameter not found in the list.
extractFromDerivedParams paramName SDPNil = Left $ "Parameter '" <> paramName <> "' not found in derived parameters."
-- Recursive case: Check the head of the list.
extractFromDerivedParams paramName (SDPCons nameProxy value rest) =
  let currentParamName = pack $ symbolVal nameProxy
  in if currentParamName == paramName
     then case cast value of
       Just typedValue -> Right typedValue
       Nothing -> Left $ "Type mismatch for parameter: " <> paramName
     else extractFromDerivedParams paramName rest

-- ============================================================================
-- 5. BATCH OPERATION SUPPORT (`Map`)
-- ============================================================================

-- | Generic helper class for converting a record ('rep') into an 'SParamTuple'.
-- Used to convert items from a list parameter into parameter tuples for mapped operations.
class GToParamTuple (rep :: Type -> Type) where
  -- | Type family associating the generic representation with its corresponding parameter list type.
  type GParams rep :: [(Symbol, Type)]
  -- | Converts the generic representation value into an 'SParamTuple'.
  gToParamTuple :: rep p -> SParamTuple (GParams rep)

-- Instances for M1 (metadata) traverse into the inner representation.
instance (GToParamTuple f) => GToParamTuple (M1 C c f) where
  type GParams (M1 C c f) = GParams f
  gToParamTuple (M1 x) = gToParamTuple x

instance (GToParamTuple f) => GToParamTuple (M1 D d f) where
  type GParams (M1 D d f) = GParams f
  gToParamTuple (M1 x) = gToParamTuple x

-- Instance for :*: (product type / record fields) combines the parameter tuples from both sides.
instance (GToParamTuple a, GToParamTuple b) => GToParamTuple (a :*: b) where
  -- Append the parameter list types.
  type GParams (a :*: b) = GParams a ++ GParams b
  -- Convert both sides and append the resulting tuples.
  gToParamTuple (a :*: b) = append (gToParamTuple a) (gToParamTuple b)
    where
      -- Helper to append SParamTuples, preserving type information.
      append :: SParamTuple l -> SParamTuple r -> SParamTuple (l ++ r)
      append STupleNil r           = r
      append (STupleCons x xs) r = STupleCons x (append xs r)

-- Instance for S (selector metadata) and K1 (field value). This is the base case for a single field.
instance (KnownSymbol name, Typeable t, ToData t, Eq t) => GToParamTuple (M1 S ('MetaSel ('Just name) u s l) (Rec0 t)) where
  -- The parameter list type is a single entry with the field's name and type.
  type GParams (M1 S ('MetaSel ('Just name) u s l) (Rec0 t)) = '[ '(name, t) ]
  -- The parameter tuple contains just the field's value.
  gToParamTuple (M1 (K1 x)) = STupleCons x STupleNil

-- | Type class indicating that a type @a@ can be used as the element type
-- for a list parameter in a 'Modsefa.Core.Foundation.Types.Map' operation.
-- Requires 'Typeable' and 'Generic'. Provides 'toMappedParamTuple'.
class (Typeable a, Generic a) => Mappable (a :: Type) where
  -- | The parameter list type (@[(Symbol, Type)]@) corresponding to the fields of @a@.
  -- Defaults to using 'GParams' on the generic representation.
  type MappedParams a :: [(Symbol, Type)]
  type MappedParams a = GParams (Rep a)

  -- | Converts a value of type @a@ into an 'SParamTuple' suitable for passing
  -- to the mapped operation. Uses 'gToParamTuple' by default.
  toMappedParamTuple :: a -> SParamTuple (MappedParams a)
  default toMappedParamTuple :: (GToParamTuple (Rep a), MappedParams a ~ GParams (Rep a)) => a -> SParamTuple (MappedParams a)
  toMappedParamTuple = gToParamTuple . from

-- | Type-level append for parameter lists (@[(Symbol, Type)]@).
-- Used in the 'GToParamTuple' instance for ':*:'.
type family (l :: [(Symbol, Type)]) ++ (r :: [(Symbol, Type)]) :: [(Symbol, Type)] where
  '[] ++ r = r
  (h ': t) ++ r = h ': (t ++ r)