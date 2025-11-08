{-|
Module      : Modsefa.Core.Singletons.Types
Description : Singleton type definitions for Modsefa specifications.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC (requires GADTs, DataKinds, TypeFamilies, etc.)

This module contains the core singleton type definitions (using GADTs) that
bring type-level Modsefa specifications ('AppSpec', 'ValidatorSpec', 'TypedActionSpec', etc.)
to the value level. These singletons enable runtime introspection, analysis,
validation, and code generation based on the type-level structure.

Each singleton type mirrors a corresponding type from "Modsefa.Core.Foundation.Types"
or related modules, carrying type information (often via proxies or constraints)
that guides runtime behavior.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Singleton type definitions for the Modsefa library
--
-- This module contains pure singleton type definitions that carry type-level
-- information to runtime. These types enable type-safe code generation and
-- runtime analysis of type-level specifications.
--
-- Organization:
--   01. Core Application Singletons
--   02. Parameter & Derivation Singletons
--   03. Action & Operation Singletons
--   04. Field & Value Singletons
--   05. Constraint Singletons
--   06. State & Reference Singletons
--   07. Version & Instance Singletons
--   08. Runtime Instance Types
--   09. Generic Classes
--   10. Field Extraction Utilities
--   11. Show Instances For Singleton Types
--   12. Field Resolution Functions
--   13. Generic Enum Conversion
module Modsefa.Core.Singletons.Types
  ( -- * Core Application Singletons
    SAppSpec(..)
  , SValidatorList(..)
  , SValidator(..)
  , SAppStateList(..)
  , SInitialAppState(..)
  , SActionTransitionList(..)
  , SActionTransition(..)

    -- * Parameter & Derivation Singletons
  , SParamList(..)
  , SAppInstanceParamList(..)
  , SParamDerivationList(..)
  , SParamDerivation(..)
  , SDerivationSource(..)

    -- * Action & Operation Singletons
  , SActionSpec(..)
  , SActionSpecList(..)
  , SActionStep(..)
  , SActionStepList(..)
  , SOperationList(..)
  , SOperation(..)

    -- * Field & Value Singletons
  , SFieldSpecList(..)
  , SFieldSpec(..)
  , STypedValue(..)

    -- * Constraint Singletons
  , SConstraintList(..)
  , SConstraint(..)
  , SCollectionConstraintList(..)
  , SCollectionConstraint(..)

    -- * State & Reference Singletons
  , SomeStateType(..)
  , SomeValidator(..)
  , SomeConstraintList(..)
  , SStateList(..)
  , SStateRef(..)
  , SPredicate(..)

    -- * Version & Instance Singletons
  , SPlutusVersion(..)
  , SInstanceType(..)
  , SParamTuple(..)

    -- * Runtime Instance Types
  , SInstanceParams(..)
  , SAppInstance(..)

    -- * Generic Classes & Utilities  
  , GBuildDatumFromSpecs(..)
  , MetaSelName
  , FromEnumValue (..)
  , extractFieldFromDatum
  , extractFieldFromReferencedUTxO
  ) where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.State (get)
import Data.Kind (Type)
import Data.List (elemIndex)
import Data.Map (toList)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text, pack)
import GHC.TypeLits (KnownNat, KnownSymbol, Symbol, natVal, symbolVal)
import Data.Typeable (Typeable, cast, eqT, typeRep, type (:~:)(Refl))
import GHC.Generics
  ( C1, D, Generic (from, to), K1(K1), M1(..), Meta(..), Rep, S, U1(U1), (:+:) (..)
  , (:*:) ((:*:))
  )

import GeniusYield.Types
  ( GYNetworkId, GYOutDatum (..), GYProviders, GYUTxO(utxoOutDatum)
  , PlutusVersion(..)
  )
import PlutusLedgerApi.V3 (POSIXTime(POSIXTime), ToData(toBuiltinData))
import PlutusTx (FromData (..))

import Modsefa.Core.Foundation
  ( ActionStep(..), AppSpec(..), CollectionConstraint(..), DerivationSource(..)
  , FieldSpec(..), GExtractField(..), GetStateData, GetStateName, InstanceType(..)
  , MetaSelName, ParamDerivation(..), ParamsToValue, ResolveInstanceParamList
  , SomeFieldValue(..), SomeStatedUTxO(..), SStateType(..), StateRepresentable
  , StateType, TypedActionSpec(..), TypedConstraint(..), TypedOperation(..)
  , TypedPredicate(And, FieldEquals), TypedStateRef(..), TypedValue(..)
  , ValidatorDef(..), ValidatorSpec(..)
  )
import Modsefa.Core.Transaction.Context
  ( DerivationContext, OperationResult (ORCreate, ORReference), TxBuilder
  , TxBuilderContext (tbcCurrentTime, tbcLetResults)
  )


-- ============================================================================
-- 1. CORE APPLICATION SINGLETONS
-- ============================================================================

-- | Singleton GADT representing a complete 'AppSpec' at the value level.
-- Holds singletons for all components of the application specification.
-- The @app@ type parameter links it to the specific 'AppSpec' instance.
data SAppSpec (app :: Type) where
  SAppSpec :: ( AppSpec app
              , KnownSymbol (InitialAppState app) -- Ensures initial state name is known
              ) => SValidatorList (Validators app)                   -- ^ Singleton for the list of validators.
                -> SAppStateList (AppStates app)                     -- ^ Singleton for application-level state names.
                -> SInitialAppState (InitialAppState app)            -- ^ Singleton for the initial application state name.
                -> SActionTransitionList app (ActionTransitions app) -- ^ Singleton for action transitions.
                -> SAppInstanceParamList (AppInstanceParameters app) -- ^ Singleton for application instance parameter definitions.
                -> SParamDerivationList (ParameterDerivations app)   -- ^ Singleton for parameter derivation rules.
                -> SAppSpec app

-- | Singleton GADT for a type-level list of 'ValidatorDef's.
-- Represents the 'Validators' list from an 'AppSpec'.
data SValidatorList (validators :: [ValidatorDef]) where
  SVNil  :: SValidatorList '[] -- ^ Represents an empty list of validators.
  SVCons :: (ValidatorSpec v, Typeable v) -- Ensures validator constraints are available
         => SValidator v -- ^ Singleton for the head validator.
         -> SValidatorList rest -- ^ Singleton for the rest of the validator list.
         -> SValidatorList ('Validator v ': rest) -- ^ Constructs the singleton list type.

-- | Singleton GADT for an individual validator type @v@ satisfying 'ValidatorSpec'.
-- Carries singletons representing the validator's parameters, managed states, etc. 
data SValidator (v :: Type) where
  SValidator :: ( ValidatorSpec v
                , KnownSymbol (ValidatorAppName v) -- Ensures validator name is known
                ) => SParamList (Params v) -- ^ Singleton for the validator's parameters ('Params').
                  -> SStateList (ManagedStates v) -- ^ Singleton for the states managed by the validator ('ManagedStates').
                  -> SPlutusVersion (ValidatorPlutusVersion v) -- ^ Singleton for the Plutus version.
                  -> SInstanceType (ValidatorInstanceType v) -- ^ Singleton for the validator's instance type.
                  -> SValidator v

-- | Singleton GADT for a type-level list of 'Symbol's representing 'AppStates'.
data SAppStateList (states :: [Symbol]) where
  SASLNil  :: SAppStateList '[] -- ^ Represents an empty list of app states.
  SASLCons :: (KnownSymbol state) -- Ensures state name is known
           => Proxy state -- ^ Proxy for the head state name.
           -> SAppStateList rest -- ^ Singleton for the rest of the state list.
           -> SAppStateList (state ': rest) -- ^ Constructs the singleton list type.

-- | Singleton GADT for the 'InitialAppState' 'Symbol'.
data SInitialAppState (state :: Symbol) where
  SInitialAppState :: (KnownSymbol state) => Proxy state -> SInitialAppState state

-- | Singleton GADT for the list of 'ActionTransitions' in an 'AppSpec'.
data SActionTransitionList (app :: Type) (transitions :: [(TypedActionSpec app, Symbol, Symbol)]) where
  SATLNil  :: SActionTransitionList app '[] -- ^ Represents an empty list of transitions.
  SATLCons :: SActionTransition app transition -- ^ Singleton for the head transition.
           -> SActionTransitionList app rest -- ^ Singleton for the rest of the transition list.
           -> SActionTransitionList app (transition ': rest) -- ^ Constructs the singleton list type.

-- | Singleton GADT for an individual action transition tuple.
data SActionTransition (app :: Type) (transition :: (TypedActionSpec app, Symbol, Symbol)) where
  SActionTransition :: ( KnownSymbol from, KnownSymbol to) -- Ensures state names are known
                    => SActionSpec app spec -- ^ Singleton for the 'TypedActionSpec' triggering the transition.
                    -> Proxy from -- ^ Proxy for the 'from' application state name.
                    -> Proxy to -- ^ Proxy for the 'to' application state name.
                    -> SActionTransition app '(spec, from, to)

-- ============================================================================
-- 2. PARAMETER & DERIVATION SINGLETONS
-- ============================================================================

-- | Singleton GADT for a type-level parameter list (@[(Symbol, Type)]@).
-- Used for both 'Params' in 'ValidatorSpec' and 'ActionSpecParameters'.
data SParamList (params :: [(Symbol, Type)]) where
  SPNil  :: SParamList '[] -- ^ Represents an empty parameter list.
  SPCons :: (KnownSymbol name, Typeable t) -- Ensures name is known and type is representable
         => Proxy name -- ^ Proxy for the parameter name.
         -> Proxy t -- ^ Proxy for the parameter type.
         -> SParamList rest -- ^ Singleton for the rest of the parameter list.
         -> SParamList ('(name, t) ': rest) -- ^ Constructs the singleton list type.

-- | Singleton GADT for the 'AppInstanceParameters' list (@[(Symbol, Symbol)]@).
-- Links validator names to parameter names required at application instantiation.
data SAppInstanceParamList (params :: [(Symbol, Symbol)]) where
  SAPNil  :: SAppInstanceParamList '[] -- ^ Represents an empty instance parameter list.
  SAPCons :: (KnownSymbol vName, KnownSymbol param) -- Ensures names are known
          => Proxy vName -- ^ Proxy for the validator name ('ValidatorAppName').
          -> Proxy param -- ^ Proxy for the parameter name within that validator's 'Params'.
          -> SAppInstanceParamList rest -- ^ Singleton for the rest of the list.
          -> SAppInstanceParamList ('(vName, param) ': rest) -- ^ Constructs the singleton list type.

-- | Singleton GADT for the list of 'ParamDerivation' rules in an 'AppSpec'.
data SParamDerivationList (derivations :: [ParamDerivation]) where
  SPDLNil :: SParamDerivationList '[] -- ^ Represents an empty list of derivation rules.
  SPDLCons :: SParamDerivation derivation -- ^ Singleton for the head derivation rule.
           -> SParamDerivationList rest -- ^ Singleton for the rest of the list.
           -> SParamDerivationList (derivation ': rest) -- ^ Constructs the singleton list type.

-- | Singleton GADT for an individual 'ParamDerivation' rule.
data SParamDerivation (derivation :: ParamDerivation) where
  SDeriveParam :: (KnownSymbol paramName) -- Ensures derived parameter name is known
               => Proxy paramName -- ^ Proxy for the name given to the derived value.
               -> SDerivationSource source -- ^ Singleton representing the source of the derivation
               -> SParamDerivation ('DeriveParam paramName source)

-- | Singleton GADT for a 'DerivationSource'. 
data SDerivationSource (source :: DerivationSource t) where
  -- | Represents deriving an 'Address' from another validator's script address.
  SValidatorAddress :: KnownSymbol validator -- Ensures source validator name is known
                    => Proxy validator -- ^ Proxy for the source validator's 'ValidatorAppName'.
                    -> SDerivationSource ('ValidatorAddress validator)
  -- | Represents deriving a 'ScriptHash' from another validator's script hash.
  SValidatorHash    :: KnownSymbol validator -- Ensures source validator name is known
                    => Proxy validator -- ^ Proxy for the source validator's 'ValidatorAppName'.
                    -> SDerivationSource ('ValidatorHash validator)

-- ============================================================================
-- 3. ACTION & OPERATION SINGLETONS
-- ============================================================================

-- | Singleton GADT for an 'ActionStep'. Mirrors the structure of 'ActionStep'.
data SActionStep (step :: ActionStep) where
  -- | Represents a simple 'Op' step containing an 'SOperation'.
  SOp  :: SOperation op -> SActionStep ('Op op)
  -- | Represents a 'Let' step, holding a 'Proxy' for the label and the 'SOperation'.
  SLet :: (KnownSymbol label) => Proxy label -> SOperation op -> SActionStep ('Let label op)
  -- | Represents a 'Map' step, holding the 'SOperation' to map, a 'Proxy' for the list parameter name,
  -- and an 'SCollectionConstraintList'.
  SMap :: (KnownSymbol param)
       => SOperation op
       -> Proxy param
       -> SCollectionConstraintList constraints
       -> SActionStep ('Map op param constraints)

-- | Singleton GADT for a list of 'ActionStep's.
data SActionStepList (steps :: [ActionStep]) where
  ASSLNil  :: SActionStepList '[] -- ^ Represents an empty list of steps.
  ASSLCons :: SActionStep step -> SActionStepList rest -> SActionStepList (step ': rest) -- ^ Constructs the singleton list type.

-- | Singleton GADT for a 'TypedActionSpec'. Carries singletons for the action's components.
-- The @app@ parameter links it to the 'AppSpec'.
data SActionSpec (app :: Type) (spec :: TypedActionSpec app) where
  SActionSpec :: ( KnownSymbol name -- Ensures action name is known
                 ) => Proxy name -- ^ Proxy for the action's name ('ActionSpecName').
                   -> SActionStepList steps -- ^ Singleton for the action's steps ('ActionSpecSteps').
                   -> SConstraintList constraints -- ^ Singleton for the action's constraints ('ActionSpecConstraints').
                   -> SParamList params -- ^ Singleton for the action's parameters ('ActionSpecParameters').
                   -> SActionSpec app ('ActionSpec @app name steps constraints params)

-- | Singleton GADT for a list of 'TypedActionSpec's.
data SActionSpecList (actions :: [TypedActionSpec app]) where
  SASNil  :: SActionSpecList '[] -- ^ Represents an empty list of action specs.
  SASCons :: SActionSpec app spec -> SActionSpecList rest -> SActionSpecList (spec ': rest) -- ^ Constructs the singleton list type.

-- | Singleton GADT for a list of 'TypedOperation's.
data SOperationList (ops :: [TypedOperation]) where
  SOLNil  :: SOperationList '[] -- ^ Represents an empty list of operations.
  SOLCons :: SOperation op -> SOperationList rest -> SOperationList (op ': rest) -- ^ Constructs the singleton list type.

-- | Singleton GADT for an individual 'TypedOperation'.
-- Each constructor carries the necessary singletons and constraints for its corresponding operation.
data SOperation (op :: TypedOperation) where
  -- | Singleton for a 'Create' operation. Requires 'StateRepresentable' and 'GBuildDatumFromSpecs'.
  SCreate :: (StateRepresentable st, GBuildDatumFromSpecs (Rep (GetStateData st)))
          => SStateType st -- ^ Singleton for the 'StateType' being created.
          -> SFieldSpecList fields -- ^ Singleton for the field assignments.
          -> SConstraintList constraints -- ^ Singleton for operation-specific constraints.
          -> SOperation ('Create fields constraints) -- Adjusted to match GADT constructor
  -- | Singleton for an 'Update' operation. Requires 'StateRepresentable' and 'GBuildDatumFromSpecs'.
  SUpdate :: (StateRepresentable st, GBuildDatumFromSpecs (Rep (GetStateData st)))
          => SStateRef st ref -- ^ Singleton for the 'TypedStateRef' identifying the state to update.
          -> SFieldSpecList fields -- ^ Singleton for the field modifications.
          -> SConstraintList constraints -- ^ Singleton for operation-specific constraints.
          -> SOperation ('Update ref fields constraints)
  -- | Singleton for a 'Delete' operation. Requires 'StateRepresentable'.
  SDelete :: StateRepresentable st
          => SStateRef st ref -- ^ Singleton for the 'TypedStateRef' identifying the state to delete.
          -> SConstraintList constraints -- ^ Singleton for operation-specific constraints.
          -> SOperation ('Delete ref constraints)
  -- | Singleton for a 'Reference' operation. Requires 'StateRepresentable'.
  SReference :: StateRepresentable st
             => SStateRef st ref -- ^ Singleton for the 'TypedStateRef' identifying the state to reference.
             -> SConstraintList constraints -- ^ Singleton for operation-specific constraints.
             -> SOperation ('Reference ref constraints)

-- ============================================================================
-- 4. FIELD & VALUE SINGLETONS
-- ============================================================================

-- | Singleton GADT for a list of 'FieldSpec's.
data SFieldSpecList (fields :: [FieldSpec]) where
  SFSNil  :: SFieldSpecList '[] -- ^ Represents an empty list of field specs
  SFSCons :: SFieldSpec field -> SFieldSpecList rest -> SFieldSpecList (field ': rest) -- ^ Constructs the singleton list type.

-- | Singleton GADT for an individual 'FieldSpec'.
data SFieldSpec (field :: FieldSpec) where
  -- | Singleton for 'SetTo', holding proxies/singletons for the field name and 'TypedValue'.
  SSetTo :: (KnownSymbol name) => Proxy name -> STypedValue value -> SFieldSpec ('SetTo name value)
  -- | Singleton for 'Preserve', holding a proxy for the field name.
  SPreserve :: (KnownSymbol name) => Proxy name -> SFieldSpec ('Preserve name)

-- | Singleton GADT for a 'TypedValue'. Mirrors the structure of 'TypedValue'.
data STypedValue (value :: TypedValue) where
  -- | Singleton for 'ParamValue', holding a proxy for the parameter name.
  SParamValue :: (KnownSymbol name) => Proxy name -> STypedValue ('ParamValue name)
  -- | Singleton for 'EnumValue', holding proxies for the enum type and constructor name. Includes necessary constraints.
  SEnumValue :: (Typeable t, FromEnumValue t, KnownSymbol s, ToData t, Eq t) => Proxy t -> Proxy s -> STypedValue ('EnumValue t s)
  -- | Singleton for 'IntValue', holding a proxy for the type-level natural number.
  SIntValue :: (KnownNat n) => Proxy n -> STypedValue ('IntValue n)
  -- | Singleton for 'StateFieldValue', holding proxies for the 'Let' label and field name.
  SStateFieldValue :: (KnownSymbol label, KnownSymbol field) => Proxy label -> Proxy field -> STypedValue ('StateFieldValue label field)
  -- | Singleton for 'CurrentTime'.
  SCurrentTime :: STypedValue 'CurrentTime
  -- | Singleton for 'AddValue', holding singletons for the two operand values.
  SAddValue :: STypedValue v1 -> STypedValue v2 -> STypedValue ('AddValue v1 v2)
  -- | Singleton for 'SubtractValue', holding singletons for the two operand values.
  SSubtractValue :: STypedValue v1 -> STypedValue v2 -> STypedValue ('SubtractValue v1 v2)
  -- | Singleton for 'MultiplyValue', holding singletons for the two operand values.
  SMultiplyValue :: STypedValue v1 -> STypedValue v2 -> STypedValue ('MultiplyValue v1 v2)
  -- | Singleton for 'DivideValue', holding singletons for the two operand values.
  SDivideValue :: STypedValue v1 -> STypedValue v2 -> STypedValue ('DivideValue v1 v2)

-- ============================================================================
-- 5. CONSTRAINT SINGLETONS
-- ============================================================================

-- | Singleton GADT for a list of 'TypedConstraint's.
data SConstraintList (constraints :: [TypedConstraint]) where
  SCLNil  :: SConstraintList '[] -- ^ Represents an empty list of constraints.
  SCLCons :: SConstraint constraint -> SConstraintList rest -> SConstraintList (constraint ': rest) -- ^ Constructs the singleton list type.

-- | Singleton GADT for an individual 'TypedConstraint'. Mirrors the structure of 'TypedConstraint'.
data SConstraint (constraint :: TypedConstraint) where
  -- | Singleton for 'MustSpendValidatorParam'. Holds proxies for validator and parameter names
  SMustSpendValidatorParam :: (KnownSymbol vName, KnownSymbol param)
                           => Proxy vName -> Proxy param
                           -> SConstraint ('MustSpendValidatorParam vName param)
  -- | Singleton for 'MustSpendActionParam'. Holds a proxy for the parameter name.
  SMustSpendActionParam :: (KnownSymbol param)
                        => Proxy param
                        -> SConstraint ('MustSpendActionParam param)
  -- | Singleton for 'MustBeSignedByState'. Holds the 'SStateRef' and field name proxy. Includes necessary constraints.
  SMustBeSignedByState ::
    ( StateRepresentable st
    , Generic (GetStateData st)
    , GExtractField (Rep (GetStateData st))
    , KnownSymbol field
    ) => SStateRef st ref -> Proxy field -> SConstraint ('MustBeSignedByState ref field)
  -- | Singleton for 'MustNotExist'. Holds the 'SStateRef'.
  SMustNotExist :: SStateRef st ref -> SConstraint ('MustNotExist ref)
  -- | Singleton for 'MustBeSignedByParam'. Holds a proxy for the parameter name.
  SMustBeSignedByParam :: (KnownSymbol param)
                        => Proxy param
                        -> SConstraint ('MustBeSignedByParam param)
  -- | Singleton for 'MustAddToAggregateState'. Holds the 'SStateType' and 'STypedValue'.
  SMustAddToAggregateState :: (StateRepresentable st)
                       => SStateType st
                       -> STypedValue value
                       -> SConstraint ('MustAddToAggregateState st value)
  -- | Singleton for 'MustWithdrawFromAggregateState'. Holds 'SStateType' and value/address 'STypedValue's.
  SMustWithdrawFromAggregateState :: (StateRepresentable st)
                                => SStateType st
                                -> STypedValue value
                                -> STypedValue address
                                -> SConstraint ('MustWithdrawFromAggregateState st value address)

-- | Singleton GADT for a list of 'CollectionConstraint's (used within 'SMap').
data SCollectionConstraintList (constraints :: [CollectionConstraint]) where
  SCCNil  :: SCollectionConstraintList '[] -- ^ Represents an empty list.
  SCCCons :: SCollectionConstraint constraint -> SCollectionConstraintList rest -> SCollectionConstraintList (constraint ': rest) -- ^ Constructs the singleton list type.

-- | Singleton GADT for a single 'CollectionConstraint'.
data SCollectionConstraint (constraint :: CollectionConstraint) where
  -- | Singleton for 'MustHaveUniqueField'. Holds a proxy for the field name.
  SMustHaveUniqueField :: (KnownSymbol field) => Proxy field -> SCollectionConstraint ('MustHaveUniqueField field)

-- ============================================================================
-- 6. STATE & REFERENCE SINGLETONS
-- ============================================================================

-- | Existential wrapper for an 'SStateType', hiding the specific state type @st@
-- but preserving the 'StateRepresentable' constraint. Useful for heterogeneous lists or maps.
data SomeStateType where
  SomeStateType :: (StateRepresentable st) => SStateType st -> SomeStateType

-- | Existential wrapper for an 'SValidator', hiding the specific validator type @v@
-- but preserving 'ValidatorSpec' and 'Typeable' constraints.
data SomeValidator where
  SomeValidator :: (ValidatorSpec v, Typeable v) => SValidator v -> SomeValidator

-- | Existential wrapper for an 'SConstraintList', hiding the specific list type @constraints@. 
data SomeConstraintList where
  SomeConstraintList :: SConstraintList constraints -> SomeConstraintList

-- | Singleton GADT for a list of 'StateType's (e.g., 'ManagedStates').
data SStateList (states :: [StateType]) where
  SSNil  :: SStateList '[] -- ^ Represents an empty list of states.
  SSCons :: (StateRepresentable st) -- Ensures state is representable
         => SStateType st -> SStateList rest -> SStateList (st ': rest) -- ^ Constructs the singleton list type.

-- | Singleton GADT for a 'TypedStateRef'. Mirrors the structure of 'TypedStateRef'.
data SStateRef (st :: StateType) (ref :: TypedStateRef st) where
  -- | Singleton for 'TypedTheOnlyInstance'. Holds the 'SStateType'.
  STypedTheOnlyInstance :: SStateType st -> SStateRef st 'TypedTheOnlyInstance
  -- | Singleton for 'TypedUniqueWhere'. Holds the 'SStateType' and 'SPredicate'.
  STypedUniqueWhere :: SStateType st -> SPredicate pred -> SStateRef st ('TypedUniqueWhere pred)
  -- | Singleton for 'TypedAny'. Holds the 'SStateType'.
  STypedAny :: SStateType st -> SStateRef st 'TypedAny
  -- | Singleton for 'TypedAnyWhere'. Holds the 'SStateType' and 'SPredicate'.
  STypedAnyWhere :: SStateType st -> SPredicate pred -> SStateRef st ('TypedAnyWhere pred)
  -- | Singleton for 'TypedByLabel'. Holds the 'SStateType' and label proxy.
  STypedByLabel :: KnownSymbol label => SStateType st -> Proxy label -> SStateRef st ('TypedByLabel label)

-- | Singleton GADT for a 'TypedPredicate'. Mirrors the structure of 'TypedPredicate'.
data SPredicate (pred :: TypedPredicate st) where
  -- | Singleton for 'FieldEquals'. Holds field name proxy and 'STypedValue'.
  SFieldEquals :: (KnownSymbol field) => Proxy field -> STypedValue value -> SPredicate ('FieldEquals field value)
  -- | Singleton for 'And'. Holds singletons for the two sub-predicates.
  SAnd         :: SPredicate p1 -> SPredicate p2 -> SPredicate ('And p1 p2)

-- ============================================================================
-- 7. VERSION & INSTANCE SINGLETONS
-- ============================================================================

-- | Singleton GADT for 'PlutusVersion'.
data SPlutusVersion (pv :: PlutusVersion) where
  SPlutusV1 :: SPlutusVersion 'PlutusV1
  SPlutusV2 :: SPlutusVersion 'PlutusV2
  SPlutusV3 :: SPlutusVersion 'PlutusV3

-- | Singleton GADT for 'InstanceType'.
data SInstanceType (it :: InstanceType) where
  SSingleInstance :: SInstanceType 'SingleInstance
  SMultiInstance :: SInstanceType 'MultiInstance

-- | Value-level heterogeneous tuple representing resolved action parameters (@[(Symbol, Type)]@).
-- Used to pass concrete parameter values during transaction building.
data SParamTuple (params :: [(Symbol, Type)]) where
  STupleNil  :: SParamTuple '[] -- ^ Represents an empty parameter tuple (for actions/validators with no parameters).
  STupleCons :: (Typeable t, ToData t, Eq t) -- Constraints needed for processing/using the value
             => t -- ^ The value of the head parameter.
             -> SParamTuple rest -- ^ Tuple containing the rest of the parameter values.
             -> SParamTuple ('(name, t) ': rest) -- ^ Constructs the typed tuple.

-- ============================================================================
-- 8. RUNTIME INSTANCE TYPES
-- ============================================================================

-- | Holds the resolved value-level instance parameters for a specific application instance.
-- The type of the contained value is determined by 'ResolveInstanceParamList' applied to the 'AppSpec'.
data SInstanceParams app where
  SInstanceParams :: ( Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
                     , Show (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
                     )
                  => ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app))
                  -> SInstanceParams app

-- | Represents a fully instantiated Modsefa application at runtime.
-- Combines the application's singleton specification ('SAppSpec') with its resolved instance parameters ('SInstanceParams').
data SAppInstance app = SAppInstance
  { appSpec :: SAppSpec app -- ^ The singleton specification of the application.
  , instanceParams :: SInstanceParams app -- ^ The resolved instance parameters for this specific instance.
  }

-- ============================================================================
-- 9. GENERIC CLASSES
-- ============================================================================

-- | Generic class for building a state datum record ('rep') from a list of 'SFieldSpec's.
-- Used internally by transaction building logic ('buildDatumFromFieldSpecs', 'buildUpdatedDatumFromFieldSpecs').
class GBuildDatumFromSpecs (rep :: Type -> Type) where
  -- | Traverses the generic representation and field specifications to construct the datum.
  gBuildDatumFromSpecs ::
    SFieldSpecList fields -> -- ^ Singleton list of field specifications ('SetTo', 'Preserve').
    SParamTuple params -> -- ^ Value-level tuple of action parameters.
    [Text] -> -- ^ List of action parameter names (for indexed access).
    DerivationContext -> -- ^ Resolved derived parameter values.
    GYNetworkId -> -- ^ Current network ID.
    GYProviders -> -- ^ Blockchain query providers.
    Maybe (rep p) -> -- ^ 'Just' the existing generic representation for 'Update', 'Nothing' for 'Create'.
    TxBuilder pv (Either Text (rep p)) -- ^ Result within the 'TxBuilder' monad.

-- Handle datatype and constructor metadata
instance {-# OVERLAPPABLE #-} GBuildDatumFromSpecs f => GBuildDatumFromSpecs (M1 i c f) where
  gBuildDatumFromSpecs fieldSpecs params actionParamNames derivationContext networkId providers mOldRep = do
    result <- gBuildDatumFromSpecs fieldSpecs params actionParamNames derivationContext networkId providers (unM1 <$> mOldRep)
    return $ M1 <$> result

-- Handle product types (multiple fields)
instance {-# OVERLAPPING #-} (GBuildDatumFromSpecs a, GBuildDatumFromSpecs b) => GBuildDatumFromSpecs (a :*: b) where
  gBuildDatumFromSpecs fieldSpecs params actionParamNames derivationContext networkId providers mOldRep = do
    leftResult <- gBuildDatumFromSpecs fieldSpecs params actionParamNames derivationContext networkId providers ((\(x :*: _) -> x) <$> mOldRep)
    case leftResult of
      Left err -> return $ Left err
      Right leftRep -> do
        rightResult <- gBuildDatumFromSpecs fieldSpecs params actionParamNames derivationContext networkId providers ((\(_ :*: y) -> y) <$> mOldRep)
        case rightResult of
          Left err -> return $ Left err
          Right rightRep -> return $ Right (leftRep :*: rightRep)

-- Handle individual fields (the core logic)
instance forall i sel a.
  ( KnownSymbol (MetaSelName sel)
  , Typeable a
  , FromData a
  , ToData a
  ) => GBuildDatumFromSpecs (M1 S sel (K1 i a)) where
  gBuildDatumFromSpecs fieldSpecs params actionParamNames derivationContext networkId providers mOldRep = do
    let fieldName = pack $ symbolVal (Proxy @(MetaSelName sel))
    liftIO $ putStrLn $ "Resolving field: " ++ show fieldName

    -- Pass derivationContext to the call
    fieldValue <- resolveFieldFromSpecs fieldName fieldSpecs params actionParamNames derivationContext networkId providers mOldRep
    case fieldValue of
      Left err -> return $ Left err
      Right value -> return $ Right (M1 (K1 value))

instance GBuildDatumFromSpecs U1 where
  gBuildDatumFromSpecs _ _ _ _ _ _ _ = return $ Right U1

-- ============================================================================
-- 10. FIELD EXTRACTION UTILITIES
-- ============================================================================

-- | Top-level function to extract a field's value ('SomeFieldValue') from a state datum using generics.
-- Requires 'StateRepresentable' and 'GExtractField' constraints for the state type.
extractFieldFromDatum :: forall st.
  ( StateRepresentable st
  , Generic (GetStateData st)
  , GExtractField (Rep (GetStateData st))
  )
  => Text -- ^ The name of the field to extract.
  -> GetStateData st -- ^ The state datum value.
  -> Maybe SomeFieldValue -- ^ 'Just' the field value wrapped in 'SomeFieldValue', or 'Nothing' if not found.
extractFieldFromDatum fieldName datum =
  gExtractField fieldName (from datum)

-- ============================================================================
-- 11. SHOW INSTANCES FOR SINGLETON TYPES
-- ============================================================================

-- Show instance for SomeStateType 
instance Show SomeStateType where
  show (SomeStateType sst) = "SomeStateType(" ++ showSStateType sst ++ ")"

-- Helper for showing SStateType
showSStateType :: SStateType st -> String
showSStateType (SStateType :: SStateType st) =
    "SStateType @" ++ symbolVal (Proxy @(GetStateName st))

-- Show instance for SomeValidator (preserves GADT constraints)
instance Show SomeValidator where
  show (SomeValidator _) = "SomeValidator(...)"

-- Show instance for SStateRef
instance Show (SStateRef st ref) where
  show (STypedTheOnlyInstance stateType) =
    "TheOnlyInstance(" ++ show (getStateName stateType) ++ ")"
  show (STypedUniqueWhere stateType _) =
    "UniqueWhere(" ++ show (getStateName stateType) ++ ")"
  show (STypedAny stateType) =
    "Any(" ++ show (getStateName stateType) ++ ")"
  show (STypedAnyWhere stateType _) =
    "AnyWhere(" ++ show (getStateName stateType) ++ ")"
  show (STypedByLabel stateType labelProxy) =
    "ByLabel(" ++ show (getStateName stateType) ++ ", " ++ show (symbolVal labelProxy) ++ ")"

-- Show instance for SParamDerivationList
instance Show (SParamDerivationList derivations) where
  show SPDLNil = "[]"
  show (SPDLCons derivation rest) =
    show derivation ++ " : " ++ show rest

-- Show instance for SParamDerivation
instance Show (SParamDerivation derivation) where
  show (SDeriveParam p source) =
    "SDeriveParam(" ++ show (symbolVal p) ++ ", " ++ show source ++ ")"

-- Show instance for SDerivationSource
instance Show (SDerivationSource (source :: DerivationSource t)) where
  show (SValidatorAddress p) = "SValidatorAddress(" ++ show (symbolVal p) ++ ")"
  show (SValidatorHash p)    = "SValidatorHash(" ++ show (symbolVal p) ++ ")"

-- | Helper to get the state name 'Text' from an 'SStateType'.
getStateName :: forall st. SStateType st -> Text
getStateName (SStateType :: SStateType st) = pack $ symbolVal (Proxy @(GetStateName st))

-- ============================================================================
-- 12. FIELD RESOLUTION FUNCTIONS
-- ============================================================================

-- | (Internal) Resolves the value for a single record field based on 'SFieldSpecList'.
-- Handles 'SetTo' (using 'resolveFromFieldSpec') and 'Preserve' (using existing value).
resolveFieldFromSpecs :: forall a sel i p fields params pv.
  ( Typeable a
  , FromData a
  , ToData a
  ) =>
  Text ->                           -- ^ Field name to resolve.
  SFieldSpecList fields ->          -- ^ Singleton list of all field specifications for the operation.
  SParamTuple params ->             -- ^ Action parameters tuple.
  [Text] ->                         -- ^ Action parameter names list.
  DerivationContext ->              -- ^ Resolved derived parameters.
  GYNetworkId ->                    -- ^ Network ID.
  GYProviders ->                    -- ^ Blockchain providers.
  Maybe (M1 S sel (K1 i a) p) ->    -- ^ 'Just' existing field value for 'Update', 'Nothing' for 'Create'.
  TxBuilder pv (Either Text a)      -- ^ Resulting field value or error.
resolveFieldFromSpecs fieldName fieldSpecs params actionParamNames derivationContext networkId providers mOldField =
  case findFieldSpecByName fieldName fieldSpecs of
    Nothing -> case mOldField of
      Just (M1 (K1 oldValue)) -> return $ Right oldValue
      Nothing -> return $ Left $ "No spec found for field '" <> fieldName <> "' and no existing value to preserve."
    Just (SomeFieldSpec fieldSpec) ->
      resolveFromFieldSpec (SomeFieldSpec fieldSpec) params actionParamNames derivationContext networkId providers mOldField

-- | (Internal) Finds the 'SFieldSpec' corresponding to a given field name.
findFieldSpecByName :: Text -> SFieldSpecList fields -> Maybe SomeFieldSpec
findFieldSpecByName _ SFSNil = Nothing
findFieldSpecByName name (SFSCons spec@(SSetTo fieldNameProxy _) rest) =
  if pack (symbolVal fieldNameProxy) == name
    then Just (SomeFieldSpec spec)
    else findFieldSpecByName name rest
findFieldSpecByName name (SFSCons spec@(SPreserve fieldNameProxy) rest) =
  if pack (symbolVal fieldNameProxy) == name
    then Just (SomeFieldSpec spec)
    else findFieldSpecByName name rest

-- | Existential wrapper for 'SFieldSpec'.
data SomeFieldSpec where
  SomeFieldSpec :: SFieldSpec field -> SomeFieldSpec

-- | (Internal) Extracts a field value of a specific type 'a' from a 'SomeStatedUTxO'.
extractFieldFromReferencedUTxO ::
  forall a. Typeable a =>
  Text -> -- ^ Field name to extract.
  SomeStatedUTxO -> -- ^ The stated UTxO containing the datum.
  Either Text a -- ^ Resulting value or error.
extractFieldFromReferencedUTxO fieldText (SomeStatedUTxO (_ :: SStateType st) utxo) =
  case utxoOutDatum utxo of
    GYOutDatumInline datum ->
      let builtinData = toBuiltinData datum
      in case fromBuiltinData builtinData :: Maybe (GetStateData st) of
        Nothing -> Left "Failed to parse datum from referenced state"
        Just stateData ->
          case extractFieldFromDatum @st fieldText stateData of
            Nothing -> Left $ "Field '" <> fieldText <> "' not found in referenced state"
            Just (SomeFieldValue fieldValue) ->
              case cast fieldValue of
                Just (typedValue :: a) -> Right typedValue
                Nothing -> Left $ "Type mismatch for field '" <> fieldText <> "' in referenced state. Expected " <> pack (show (typeRep (Proxy @a)))
    GYOutDatumNone -> Left "Referenced UTxO has no datum."
    GYOutDatumHash _dh -> Left "Referenced UTxO has datum hash, but inline datum is required for field extraction."

-- | (Internal) Resolves the concrete value 'a' based on a single 'SomeFieldSpec'.
-- Handles different 'STypedValue' sources ('SParamValue', 'SEnumValue', 'SStateFieldValue', etc.).
resolveFromFieldSpec :: forall a sel i p params pv.
  ( Typeable a
  , FromData a
  , ToData a
  ) =>
  SomeFieldSpec ->                -- ^ The specific field specification ('SetTo' or 'Preserve').
  SParamTuple params ->           -- ^ Action parameters tuple.
  [Text] ->                       -- ^ Action parameter names list.
  DerivationContext ->            -- ^ Resolved derived parameters.
  GYNetworkId ->                  -- ^ Network ID.
  GYProviders ->                  -- ^ Blockchain providers.
  Maybe (M1 S sel (K1 i a) p) ->  -- ^ 'Just' existing field value for 'Update', 'Nothing' for 'Create'.
  TxBuilder pv (Either Text a)    -- ^ Resulting field value or error.
resolveFromFieldSpec (SomeFieldSpec fieldSpec) params actionParamNames derivationContext _networkId _providers mOldField = case fieldSpec of
  SSetTo _ (SParamValue paramProxy) -> do
    let paramName = pack $ symbolVal paramProxy
    -- 1. Check the derivation context first.
    case lookup paramName (toList derivationContext) of
      Just (SomeFieldValue val) ->
        case cast val of
          Just typedVal -> return $ Right typedVal
          Nothing -> return $ Left $ "Derived parameter '" <> paramName <> "' has the wrong type."
      -- 2. If not found, fall back to the action parameters.
      Nothing -> return $ extractParamByName paramName actionParamNames params

  SSetTo _ (SEnumValue (_typeProxy :: Proxy t) (symbolProxy :: Proxy s)) -> do
    let enumSymbol = symbolVal symbolProxy
    -- We check that the type `t` from the spec matches the field's actual type `a`
    case eqT @a @t of
      Just Refl ->
        -- The types match, so we can now safely use `fromEnumValue` on the field's type `a`
        return $ case fromEnumValue @a enumSymbol of
          Just val -> Right val
          Nothing  -> Left $ "Internal error: EnumValue '" <> pack enumSymbol <> "' is a valid constructor for type " <> pack (show (typeRep (Proxy @a))) <> " but failed to convert at runtime."
      Nothing ->
        return $ Left $ "Type mismatch in spec: the record field has type " <> pack (show (typeRep (Proxy @a))) <> " but the 'EnumValue' was for the different type " <> pack (show (typeRep (Proxy @t)))

  SSetTo _ (SStateFieldValue labelProxy fieldProxy) -> do
    let labelText = pack $ symbolVal labelProxy
    let fieldText = pack $ symbolVal fieldProxy
    ctx <- get
    case lookup labelText (toList $ tbcLetResults ctx) of
      Nothing -> return $ Left $ "Reference label not found in context: " <> labelText
      -- This pattern match brings the GExtractField constraint into scope
      Just (ORReference (SomeStatedUTxO (_stype :: SStateType st) utxo)) -> do
        case utxoOutDatum utxo of
          GYOutDatumInline datum ->
            let builtinData = toBuiltinData datum
            in case fromBuiltinData builtinData :: Maybe (GetStateData st) of
              Nothing -> return $ Left "Failed to parse datum from referenced state"
              Just stateData ->
                case extractFieldFromDatum @st fieldText stateData of
                  Nothing -> return $ Left $ "Field '" <> fieldText <> "' not found in referenced state '" <> labelText <> "'"
                  Just (SomeFieldValue fieldValue) ->
                    case cast fieldValue of
                      Just (typedValue :: a) -> return $ Right typedValue
                      Nothing -> return $ Left $ "Type mismatch for field '" <> fieldText <> "' in referenced state. Expected " <> pack (show (typeRep (Proxy @a)))
          _ -> return $ Left "Referenced UTxO does not have an inline datum"
      Just (ORCreate _) -> return $ Left $ "Cannot reference a field from a newly created state within the same action: " <> labelText

  -- Handles integer literals from the spec
  SSetTo _ (SIntValue (intValProxy :: Proxy n)) ->
    case cast (natVal intValProxy) of
      Just val -> return $ Right val
      Nothing  -> return $ Left $ "Type mismatch: IntValue cannot be cast to target field type " <> pack (show (typeRep (Proxy @a)))

  -- Handles fetching the current time from the transaction context
  SSetTo _ SCurrentTime -> do
    ctx <- get
    let currentTime = tbcCurrentTime ctx
    case cast currentTime of
      Just val -> return $ Right val
      Nothing -> return $ Left $ "Type mismatch: CurrentTime (POSIXTime) cannot be cast to target field type " <> pack (show (typeRep (Proxy @a)))

  SSetTo _ (SAddValue innerFv1 innerFv2) -> do
    -- This handles the addition of a referenced POSIXTime to the current time.
    case (eqT @a @POSIXTime, innerFv1, innerFv2) of
      (Just Refl, SCurrentTime, SStateFieldValue labelProxy fieldProxy) -> do
        let labelText = pack $ symbolVal labelProxy
        let fieldText = pack $ symbolVal fieldProxy
        ctx <- get
        -- Resolve the nested StateFieldValue
        let mReferencedValue = do
              opResult <- lookup labelText (toList $ tbcLetResults ctx)
              case opResult of
                ORReference (SomeStatedUTxO (_ :: SStateType st) utxo) ->
                  case utxoOutDatum utxo of
                    GYOutDatumInline datum ->
                      let builtinData = toBuiltinData datum
                      in case fromBuiltinData builtinData :: Maybe (GetStateData st) of
                        Nothing -> Nothing
                        Just stateData -> extractFieldFromDatum @st fieldText stateData
                    _ -> Nothing -- Handle GYOutDatumNone, GYOutDatumHash if needed within lookup
                _ -> Nothing

        case mReferencedValue of
          Nothing -> return $ Left $ "Failed to resolve nested StateFieldValue '" <> fieldText <> "' for AddValue."
          Just (SomeFieldValue val) ->
            -- Cast to POSIXTime
            case cast val of
              Nothing -> return $ Left $ "The referenced field '" <> fieldText <> "' for AddValue was not a POSIXTime value."
              Just (POSIXTime timeToAdd) -> do
                -- Now perform the time addition
                let (POSIXTime currentTime) = tbcCurrentTime ctx
                let newTime = POSIXTime (currentTime + timeToAdd)
                return $ Right newTime
      _ -> return $ Left "This AddValue implementation only supports adding a referenced POSIXTime StateFieldValue to CurrentTime."

  SSetTo _ (SSubtractValue v1 v2) -> do
    eVal1 <- resolveValue v1
    eVal2 <- resolveValue v2
    case (eVal1, eVal2) of
      (Right (SomeFieldValue val1), Right (SomeFieldValue val2)) ->
        case (cast val1, cast val2) of
          (Just (i1 :: Integer), Just (i2 :: Integer)) ->
            case cast (i1 - i2) of
              Just result -> return $ Right result
              Nothing -> return $ Left "Type mismatch: Subtraction result could not be cast to the target field type."
          _ -> return $ Left "Unsupported types for Subtraction. Only Integer is supported."
      (Left err, _) -> return $ Left err
      (_, Left err) -> return $ Left err

  SSetTo _ (SMultiplyValue v1 v2) -> do
    eVal1 <- resolveValue v1
    eVal2 <- resolveValue v2
    case (eVal1, eVal2) of
      (Right (SomeFieldValue val1), Right (SomeFieldValue val2)) ->
        case (cast val1, cast val2) of
          (Just (i1 :: Integer), Just (i2 :: Integer)) ->
            case cast (i1 * i2) of
              Just result -> return $ Right result
              Nothing -> return $ Left "Type mismatch: Multiplication result could not be cast to the target field type."
          _ -> return $ Left "Unsupported types for Multiplication. Only Integer is supported."
      (Left err, _) -> return $ Left err
      (_, Left err) -> return $ Left err

  SSetTo _ (SDivideValue v1 v2) -> do
    eVal1 <- resolveValue v1
    eVal2 <- resolveValue v2
    case (eVal1, eVal2) of
      (Right (SomeFieldValue val1), Right (SomeFieldValue val2)) ->
        case (cast val1, cast val2) of
          (Just (i1 :: Integer), Just (i2 :: Integer)) ->
            case cast (i1 `div` i2) of
              Just result -> return $ Right result
              Nothing -> return $ Left "Type mismatch: Division result could not be cast to the target field type."
          _ -> return $ Left "Unsupported types for Division. Only Integer is supported."
      (Left err, _) -> return $ Left err
      (_, Left err) -> return $ Left err

  SPreserve _ -> do
    case mOldField of
      Just (M1 (K1 oldValue)) -> return $ Right oldValue
      Nothing -> return $ Left "Preserve specified but no existing value available"

  where
    resolveValue :: STypedValue v -> TxBuilder pv (Either Text SomeFieldValue)
    resolveValue (SStateFieldValue labelProxy' fieldProxy') = do
        let labelText' = pack $ symbolVal labelProxy'
        let fieldText' = pack $ symbolVal fieldProxy'
        ctx <- get
        case lookup labelText' (toList $ tbcLetResults ctx) of
            Just (ORReference (SomeStatedUTxO stype utxo)) ->
                case extractFieldFromReferencedUTxO @Integer fieldText' (SomeStatedUTxO stype utxo) of
                    Right intValue -> return $ Right $ SomeFieldValue intValue
                    Left err -> return $ Left err
            _ -> return $ Left $ "Could not resolve StateFieldValue for arithmetic: " <> labelText' <> "." <> fieldText'
    resolveValue (SIntValue intValProxy) = return $ Right $ SomeFieldValue (natVal intValProxy)

    -- Recursive cases for arithmetic
    resolveValue (SSubtractValue v1 v2) = applyArithmetic (-) v1 v2
    resolveValue (SMultiplyValue v1 v2) = applyArithmetic (*) v1 v2
    resolveValue (SDivideValue v1 v2) = applyArithmetic div v1 v2

    resolveValue _ = return $ Left "Unsupported TypedValue for arithmetic operation."

    -- Helper to reduce boilerplate for arithmetic operations.
    applyArithmetic :: (Integer -> Integer -> Integer) -> STypedValue v1 -> STypedValue v2 -> TxBuilder pv (Either Text SomeFieldValue)
    applyArithmetic op v1 v2 = do
        eVal1 <- resolveValue v1
        eVal2 <- resolveValue v2
        case (eVal1, eVal2) of
            (Right (SomeFieldValue val1), Right (SomeFieldValue val2)) ->
                case (cast val1, cast val2) of
                    (Just (i1 :: Integer), Just (i2 :: Integer)) -> return $ Right $ SomeFieldValue (op i1 i2)
                    _ -> return $ Left "Arithmetic operation encountered a non-Integer value."
            (Left err, _) -> return $ Left err
            (_, Left err) -> return $ Left err

-- | (Internal) Extracts a parameter value of type 'a' from an 'SParamTuple' by name.
extractParamByName :: forall a params.
  ( Typeable a
  , FromData a
  , ToData a
  ) =>
  Text ->               -- ^ Parameter name.
  [Text] ->             -- ^ List of all parameter names in order.
  SParamTuple params -> -- ^ The parameter tuple.
  Either Text a         -- ^ Resulting value or error.
extractParamByName targetName paramNames params = do
  position <- case elemIndex targetName paramNames of
    Just pos -> Right pos
    Nothing -> Left $ "Parameter not found: " <> targetName
  extractParamAtIndex position params
  where
    extractParamAtIndex :: forall params'. Int -> SParamTuple params' -> Either Text a
    extractParamAtIndex n (STupleCons val rest)
      | n == 0 = case cast val of
          Just typedVal -> Right typedVal
          Nothing -> Left "Parameter has wrong type"
      | otherwise = extractParamAtIndex (n - 1) rest
    extractParamAtIndex _ STupleNil = Left "Parameter index out of bounds"

-- ============================================================================
-- 13. GENERIC ENUM CONVERSION
-- ============================================================================

-- | Generic helper class for converting a constructor name ('String') to an enum value ('rep').
-- Used internally by 'FromEnumValue'. Operates on the 'Rep' type.
class GFromEnumValue (rep :: Type -> Type) where
  -- | Attempts the conversion based on constructor names in the 'Rep'.
  gFromEnumValue :: String -> Maybe (rep p)

-- | Type class for converting a constructor name ('String') into a value of an enumeration type 'a'.
-- Requires 'Eq a' and typically 'Generic a'. Provides a default implementation using 'GFromEnumValue'.
class (Eq a) => FromEnumValue a where
  -- | Attempts to convert the string to the enum value. Returns 'Nothing' if the string is not a valid constructor name.
  fromEnumValue :: String -> Maybe a
  -- | Default generic implementation.
  default fromEnumValue :: (Generic a, GFromEnumValue (Rep a)) => String -> Maybe a
  fromEnumValue s = to <$> gFromEnumValue s

-- For top-level datatype metadata, we recurse into the sum of constructors.
instance GFromEnumValue f => GFromEnumValue (M1 D c f) where
  gFromEnumValue s = M1 <$> gFromEnumValue s

-- For a sum of constructors (:+:), we try the left side, then the right.
instance (GFromEnumValue a, GFromEnumValue b) => GFromEnumValue (a :+: b) where
  gFromEnumValue s = (L1 <$> gFromEnumValue s) <|> (R1 <$> gFromEnumValue s)

-- For a single constructor (C1), we check if the constructor's name matches the string.
-- This is the success case for our enum conversion.
instance (KnownSymbol name) => GFromEnumValue (C1 ('MetaCons name f s) U1) where
  gFromEnumValue s = if s == symbolVal (Proxy @name)
                     then Just (M1 U1)
                     else Nothing