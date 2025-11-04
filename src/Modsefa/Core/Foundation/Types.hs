{-|
Module      : Modsefa.Core.Foundation.Types
Description : Core type definitions for the Modsefa library.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module contains the fundamental data types that form the foundation
of the type-safe smart contract DSL. All other modules build upon these
core definitions. It defines core concepts like 'StateType', 'AppSpec',
'ValidatorSpec', 'TypedActionSpec', operations ('TypedOperation'),
constraints ('TypedConstraint'), and related helpers.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Core type definitions for the Modsefa library
-- 
-- This module contains the fundamental data types that form the foundation
-- of the type-safe smart contract DSL. All other modules build upon these
-- core definitions.
--
-- Organization:
--   1. Core Primitives
--   2. State Representable Type Class
--   3. Action Specifications 
--   4. State References & Predicates
--   5. Constraints
--   6. Parameter Derivations
--   7. Generic GADTS & Utility Classes
module Modsefa.Core.Foundation.Types
  ( -- * Core Primitives
    StateType(..)
  , SStateType(..)
  , ValidatorDef(..)
  , InstanceType(..)
  , PolicySource(..)
  , StateIdentifier(..)
  , RefStrategy(..)
    
    -- * State Representation
  , StateRepresentable(..)
  , GetStateData
  , GetStateName
    
    -- * Core Type Classes
  , AppSpec(..)
  , ValidatorSpec(..)
    
    -- * Action Specifications
  , TypedActionSpec(..)
  , ActionStep(..)
  , TypedOperation(..)
  , FieldSpec(..)
  , TypedValue(..)
    
    -- * State References & Predicates
  , TypedStateRef(..)
  , TypedPredicate(..)
  , TypedFieldPath(..)
    
    -- * Constraints
  , TypedConstraint(..)
  , CollectionConstraint(..)
    
    -- * Parameter Derivations
  , ParamDerivation(..)
  , DerivationSource(..)
    
    -- * Type Families (declared in type classes)
  , InitialStateInStates
  , ValidateAppInstanceParameters
    
    -- * Helpers
  , SomeStatedUTxO(..)
  , SomeFieldValue(..)
  , GExtractField(..)
  , MetaSelName
  ) where

import Control.Applicative ((<|>))
import Data.Kind (Constraint, Type)
import Data.Text (Text, pack)
import Data.Type.Bool (If)
import Data.Type.Equality (type (==))
import GHC.Generics 
  ( Generic (Rep), K1 (K1), M1 (M1), Meta (MetaSel), Rec0, S, U1, (:*:) ((:*:))
  )
import GHC.TypeLits 
  ( ErrorMessage(..), KnownSymbol, Nat, Symbol, TypeError, symbolVal
  )
import Data.Typeable (Proxy (Proxy), Typeable)

import GeniusYield.Types (GYAssetClass, GYTokenName, GYUTxO, PlutusVersion)
import PlutusLedgerApi.V3 (Address, CurrencySymbol, ScriptHash, ToData(toBuiltinData))
import PlutusTx (FromData)


-- ============================================================================
-- 1. CORE PRIMITIVES
-- ============================================================================

-- | Fundamental state type definition.
--
-- A 'StateType' connects a symbolic name ('Symbol') with a concrete Haskell
-- data type ('Type').
--
-- @Example: 'ST "ServiceConfig" ServiceConfig@
data StateType = ST Symbol Type

-- | Singleton GADT for 'StateType'.
-- Carries 'StateRepresentable' constraint ensuring the type can be used on-chain.
data SStateType (st :: StateType) where
  SStateType :: (StateRepresentable st) => SStateType st

-- Show instance for SStateType
instance Show (SStateType st) where
  show (SStateType :: SStateType st) = 
    "SStateType(" ++ show (symbolVal (Proxy @(GetStateName st))) ++ ")"

-- | A way to lift a specific validator 'Type' (which has a 'ValidatorSpec' instance)
-- into type-level lists within an 'AppSpec' (e.g., in 'Validators').
data ValidatorDef where
  Validator :: Type -> ValidatorDef

-- | Specifies the instantiation model for a validator script within an application instance.
data InstanceType 
  = SingleInstance   -- ^ The validator script exists as a single instance for the entire application instance.
  | MultiInstance    -- ^ Multiple instances of the validator script can exist, parameterized distinctly (e.g., by customer PKH).
  deriving (Eq, Show)

-- | Policy source for state identification tokens.
data PolicySource
  = OwnPolicy          -- ^ Use the validator's own minting policy script hash as the 'CurrencySymbol'.
  | ExternalPolicy CurrencySymbol  -- ^ Use a specified external 'CurrencySymbol'.
  deriving (Eq, Show)

-- | Defines how distinct instances of a 'StateType' are identified on-chain.
data StateIdentifier
  = TokenIdentified PolicySource GYTokenName Integer  -- ^ Identified by tokens
  | AggregateAsset GYAssetClass                       -- ^ Identified by a quantity
  deriving (Eq, Show)

-- | Reference strategy allowed for a 'StateType'.
-- Determines how instances of this state can be looked up or referenced in operations.
data RefStrategy 
  = OnlyByProperty   -- ^ Can only reference by property queries
  | OnlyAsUnique     -- ^ Can only reference as unique instance
  | AnyRef           -- ^ Can reference by any method
  | NoRef            -- ^ Cannot be referenced in operations (aggregated-based states)
  deriving (Eq, Show)

-- ============================================================================
-- 2. STATE REPRESENTABLE TYPE CLASS
-- ============================================================================

-- | Type class for state types that can be represented on-chain
--
-- This defines how state types are identified and managed on the blockchain.
class ( ToData (GetStateData st)
      , FromData (GetStateData st)
      , Show (GetStateData st)
      , Eq (GetStateData st)
      , KnownSymbol (GetStateName st)
      , Typeable st
      , Typeable (GetStateData st)
      , Generic (GetStateData st)
      ) => StateRepresentable (st :: StateType) where
  -- | How this state type is identified on-chain
  stateIdentifier :: proxy st -> StateIdentifier
  
  -- | Specifies which reference strategies ('RefStrategy') are allowed for this state type.
  -- Defaults to 'AnyRef'.
  type AllowedRefStrategy st :: RefStrategy
  type AllowedRefStrategy st = 'AnyRef

-- | Type family to extract the underlying Haskell data type from a 'StateType'.
-- @GetStateData ('ST name MyRecord) = MyRecord@e
type family GetStateData (st :: StateType) :: Type where
  GetStateData ('ST _ t) = t

-- | Type family to extract the symbolic name from a 'StateType'.
-- @GetStateName ('ST "MyName" MyRecord) = "MyName"@
type family GetStateName (st :: StateType) :: Symbol where
  GetStateName ('ST name _) = name

-- | Core Application specification type class.
-- Defines the complete structure of a blockchain application including
-- its validators, states, transitions, and parameter configuration.
class 
  ( InitialStateInStates app
  , ValidateAppInstanceParameters app
  ) => AppSpec (app :: Type) where
  
  -- | List of 'ValidatorDef's participating in this application.
  type Validators app :: [ValidatorDef]
  
  -- | List of application-level state names ('Symbol') used for high-level state machine tracking.
  type AppStates app :: [Symbol]
  
  -- | The initial state ('Symbol') of the application state machine. Must be in 'AppStates'.
  type InitialAppState app :: Symbol
  
  -- | Defines valid transitions between application states triggered by specific 'TypedActionSpec's.
  -- Format: @'(ActionSpec, FromStateName, ToStateName)@
  type ActionTransitions app :: [(TypedActionSpec app, Symbol, Symbol)]
  
  -- | Instance-specific parameters for validators, linking validator names to parameter names.
  -- Format: @'(ValidatorName, ParameterName)@. These parameters are typically provided at deployment/instantiation.
  type AppInstanceParameters app :: [(Symbol, Symbol)]
  
  -- | Defines rules for automatically deriving validator parameters from other sources within the application instance.
  type ParameterDerivations app :: [ParamDerivation]

-- | Defines the specification for an individual validator script within the application.
class (KnownSymbol (ValidatorAppName v)) => ValidatorSpec (v :: Type) where
  -- | Parameter types required by this validator's script, specified as a list of '(Name, Type)'.
  type Params v :: [(Symbol, Type)]
  
  -- | 'StateType's whose lifecycle (creation, updates, deletion, aggregation) is managed by this validator script.
  type ManagedStates v :: [StateType]
  
  -- | A unique symbolic name ('Symbol') identifying this validator within the 'AppSpec'.
  type ValidatorAppName v :: Symbol
  
  -- | The Plutus script version ('PlutusVersion') used by this validator.
  type ValidatorPlutusVersion v :: PlutusVersion
  
  -- | Specifies whether this validator exists as a 'SingleInstance' or 'MultiInstance' within the application.
  type ValidatorInstanceType v :: InstanceType

-- | Constraint ensuring initial state is valid
--
-- Validates that the application's initial state is one of the declared
-- application states.
type family InitialStateInStates (app :: Type) :: Constraint where
  InitialStateInStates app = 
    If (IsElementOf (InitialAppState app) (AppStates app))
       (() :: Constraint)
       (TypeError ('Text "Initial state " ':<>: 'ShowType (InitialAppState app) 
                   ':<>: 'Text " is not in application states"))

-- | Constraint validating application instance parameters
--
-- Ensures that the instance parameters reference valid validators and
-- parameter names.
type family ValidateAppInstanceParameters (app :: Type) :: Constraint where
  ValidateAppInstanceParameters app = ValidateInstanceParamList (AppInstanceParameters app) (Validators app)

-- Helper type families for validation
type family IsElementOf (elem :: k) (list :: [k]) :: Bool where
  IsElementOf elem (elem ': _) = 'True
  IsElementOf elem (_ ': rest) = IsElementOf elem rest
  IsElementOf _ '[] = 'False

type family ValidateInstanceParamList (params :: [(Symbol, Symbol)]) (validators :: [ValidatorDef]) :: Constraint where
  ValidateInstanceParamList '[] _ = ()
  ValidateInstanceParamList ('(validatorName, paramName) ': rest) validators =
    ( ValidateValidatorHasParam validatorName paramName validators
    , ValidateInstanceParamList rest validators
    )

type family ValidateValidatorHasParam (validatorName :: Symbol) (paramName :: Symbol) (validators :: [ValidatorDef]) :: Constraint where
  ValidateValidatorHasParam validatorName paramName validators =
    If (HasValidatorParam validatorName paramName validators)
       (() :: Constraint)
       (TypeError ('Text "Validator " ':<>: 'ShowType validatorName 
                   ':<>: 'Text " does not have parameter " ':<>: 'ShowType paramName))

type family HasValidatorParam (validatorName :: Symbol) (paramName :: Symbol) (validators :: [ValidatorDef]) :: Bool where
  HasValidatorParam validatorName paramName ('Validator v ': rest) =
    If (ValidatorAppName v == validatorName)
       (IsElementOf paramName (ParamNames (Params v)))
       (HasValidatorParam validatorName paramName rest)
  HasValidatorParam _ _ '[] = 'False

type family ParamNames (params :: [(Symbol, Type)]) :: [Symbol] where
  ParamNames '[] = '[]
  ParamNames ('(name, _) ': rest) = name ': ParamNames rest

-- ============================================================================
-- 3. ACTION SPECIFICATIONS
-- ============================================================================

-- | Defines steps within an action, allowing control flow like mapping over lists.
data ActionStep where
  -- | A single, standard operation ('TypedOperation').
  Op  :: TypedOperation -> ActionStep
  
  -- | A labeled operation ('TypedOperation'), allowing its result (e.g., a referenced UTxO)
  -- to be referred to by subsequent steps or constraints using the 'Symbol' label.
  Let :: Symbol -> TypedOperation -> ActionStep

  -- | Maps an operation ('TypedOperation') over a list parameter ('Symbol').
  -- Optionally includes 'CollectionConstraint's to validate the list itself (e.g., uniqueness).
  Map :: TypedOperation         -- ^ The operation to repeat for each item in the list.
      -> Symbol                 -- ^ The name of the list-type parameter from the action's parameters.
      -> [CollectionConstraint] -- ^ Constraints applied to the collection as a whole.
      -> ActionStep

-- | Complete action specification, linking a name, steps, constraints, and parameters to an 'AppSpec'.
-- The @app@ parameter ensures actions are defined within the context of a specific application.
data TypedActionSpec (app :: Type) where
  ActionSpec 
    :: Symbol                    -- ^ The unique name identifying this action.
    -> [ActionStep]              -- ^ A sequence of 'ActionStep's defining the action's logic.
    -> [TypedConstraint]         -- ^ Validation constraints applied to the overall transaction for this action.
    -> [(Symbol, Type)]          -- ^ Parameter specifications (name and type) required by this action.
    -> TypedActionSpec app

-- | Represents fundamental operations performed on state instances within an action.
-- These typically correspond to consuming inputs and producing outputs governed by a validator script.
data TypedOperation where
  -- | Creates a new instance of a 'StateType'.
  Create 
    :: forall st name record. (st ~ 'ST name record) => -- ^ The specific 'StateType' to create.
       [FieldSpec]               -- ^ Field assignments ('FieldSpec') for the new state datum.
    -> [TypedConstraint]         -- ^ Additional constraints specific to this operation.
    -> TypedOperation
    
  -- | Updates an existing instance identified by a 'TypedStateRef'.
  Update 
    :: TypedStateRef st          -- ^ Reference to the state instance to update.
    -> [FieldSpec]               --- ^ Field modifications ('FieldSpec') for the state datum.
    -> [TypedConstraint]         -- ^ Additional constraints specific to this operation.
    -> TypedOperation
    
  -- | Deletes an existing state instance identified by a 'TypedStateRef'.
  Delete 
    :: TypedStateRef st          -- ^ Reference to the state instance to delete.
    -> [TypedConstraint]         -- ^ Additional constraints specific to this operation.
    -> TypedOperation
    
  -- | References an existing state instance (read-only) identified by a 'TypedStateRef'.
  -- Primarily used within a 'Let' step to make the state's datum available to subsequent steps.
  Reference 
    :: TypedStateRef st          -- ^ Reference to the state instance to read.
    -> [TypedConstraint]         -- ^ Additional constraints specific to this operation.
    -> TypedOperation

-- | Specification for setting or preserving field values during 'Create' or 'Update' operations.
data FieldSpec where
  -- | Sets a field ('Symbol') to a specific 'TypedValue'.
  SetTo 
    :: Symbol                    -- ^ Field name within the state's data record.
    -> TypedValue                -- ^ The value to set the field to.
    -> FieldSpec
    
  -- | Preserves the existing value of a field ('Symbol') from the input state to the output state during an 'Update'.
  Preserve 
    :: Symbol                    -- ^ Field name to preserve.
    -> FieldSpec

-- | Represents different sources for values used in 'FieldSpec' assignments.
data TypedValue where
  -- | Value originates from an action parameter identified by name ('Symbol').
  ParamValue 
    :: Symbol                    -- ^ Parameter name from the action's parameter list.
    -> TypedValue
    
  -- | A literal value belonging to an enumeration type ('Type'), identified by its constructor name ('Symbol').
  -- Requires a 'FromEnumValue' instance for the enum type.
  EnumValue 
    :: Type                      -- ^ The enumeration type itself.
    -> Symbol                    -- ^ The constructor name as a symbol.
    -> TypedValue
    
  -- | A literal integer value ('Nat').
  IntValue 
    :: Nat                       -- ^ The integer value.
    -> TypedValue
    
  -- | Value extracted from a field ('Symbol') of another state instance previously referenced
  -- in a 'Let' step identified by its label ('Symbol').
  StateFieldValue
    :: Symbol                    -- ^ The label of a preceding 'Let' step referencing a state.
    -> Symbol                    -- ^ The field name within the referenced state's datum.
    -> TypedValue

  -- | Represents the transaction's current time (lower bound of the validity range). Resolves to 'POSIXTime'.
  CurrentTime :: TypedValue

  -- | The sum of two 'TypedValue's (typically 'Integer' or 'POSIXTime').
  AddValue :: TypedValue -> TypedValue -> TypedValue

  -- | The difference between two 'TypedValue's (typically 'Integer' or 'POSIXTime').
  SubtractValue :: TypedValue -> TypedValue -> TypedValue

  -- | The product of two 'TypedValue's (typically 'Integer').
  MultiplyValue :: TypedValue -> TypedValue -> TypedValue

  -- | The integer division of two 'TypedValue's (typically 'Integer').
  DivideValue :: TypedValue -> TypedValue -> TypedValue

-- ============================================================================
-- 4. STATE REFERENCES & PREDICATES
-- ============================================================================

-- | Type-safe reference to state instances, specifying the lookup strategy.
-- The @st@ parameter ensures references are tied to a specific 'StateType'.
data TypedStateRef (st :: StateType) where
  -- | Reference to the single, unique instance of the state (only valid if 'AllowedRefStrategy' permits 'OnlyAsUnique').
  TypedTheOnlyInstance :: TypedStateRef st
  
  -- | Reference to the unique instance matching a 'TypedPredicate' (only valid if 'AllowedRefStrategy' permits 'OnlyByProperty' or 'AnyRef').
  TypedUniqueWhere 
    :: TypedPredicate st         -- ^ Predicate that must match exactly one instance.
    -> TypedStateRef st
    
  -- | References any single instance of state 'st'.
  TypedAny :: TypedStateRef st

  -- | Reference to any single instance matching a 'TypedPredicate' (only valid if 'AllowedRefStrategy' permits 'OnlyByProperty' or 'AnyRef').
  TypedAnyWhere :: TypedPredicate st -> TypedStateRef st

  -- | Reference to a state instance previously resolved in a 'Let' step, identified by its label ('Symbol').
  TypedByLabel :: Symbol -> TypedStateRef st

-- | Type-safe predicates for state queries and references, evaluated against state datums.
-- The @st@ parameter ensures predicates operate on the correct 'StateType'.
data TypedPredicate (st :: StateType) where
  -- | Checks if a field ('Symbol') equals a specific 'TypedValue'.
  FieldEquals 
    :: Symbol                    -- ^ Field name within the state's data record.
    -> TypedValue                -- ^ Expected value for comparison.
    -> TypedPredicate st
    
  -- | Checks if a field ('Symbol') is greater than a specific 'TypedValue'. (Numeric types)
  FieldGT 
    :: Symbol                    -- ^ Field name.
    -> TypedValue                -- ^ Value to compare against.
    -> TypedPredicate st
    
  -- | Checks if a field ('Symbol') is less than a specific 'TypedValue'. (Numeric types)
  FieldLT 
    :: Symbol                    -- ^ Field name.
    -> TypedValue                -- ^ Value to compare against.
    -> TypedPredicate st

  -- | Logical AND combining two 'TypedPredicate's for the same state type.
  And
    :: TypedPredicate st         -- ^ First predicate.
    -> TypedPredicate st         -- ^ Second predicate.
    -> TypedPredicate st

-- | Type-safe path linking a record type, field name, and field type.
-- Primarily used for type-level validation in constraints like 'PreserveStateField'.
data TypedFieldPath (record :: Type) (field :: Symbol) (fieldType :: Type) where
  TypedFieldPath :: TypedFieldPath record field fieldType

-- ============================================================================
-- 5. CONSTRAINTS
-- ============================================================================

-- | Validation constraints applied to transactions generated by actions.
-- These are checked during Plutus script execution.
data TypedConstraint where
  -- ** State-based constraints

  -- | Requires the transaction to be signed by a key whose 'PubKeyHash' is stored
  -- in a specific field ('Symbol') of a referenced state instance ('TypedStateRef').
  MustBeSignedByState 
    :: TypedStateRef st          -- ^ State reference containing the signer's PubKeyHash.
    -> Symbol                    -- ^ Field name holding the 'PubKeyHash'.
    -> TypedConstraint
    
  -- | Requires a specific field ('TypedFieldPath') of a state instance ('TypedStateRef')
  -- to remain unchanged during an 'Update' operation. (Currently informational, not fully enforced on-chain).
  PreserveStateField 
    :: TypedStateRef st          -- ^ State reference being updated.
    -> TypedFieldPath record field fieldType  -- ^ Path to the field that must be preserved.
    -> TypedConstraint
    
  -- | Requires a specific field ('TypedFieldPath') of a referenced state instance ('TypedStateRef')
  -- to have a specific value ('TypedValue'). (Currently informational, not fully enforced on-chain).
  RequireStateValue 
    :: TypedStateRef st          -- ^ State reference to check.
    -> TypedFieldPath record field fieldType  -- ^ Path to the field.
    -> TypedValue                -- ^ The required value.
    -> TypedConstraint
    
  -- ** Existence constraints

  -- | Requires that at least one instance matching the 'TypedStateRef' exists on-chain.
  MustExist 
    :: TypedStateRef st          -- ^ State reference to check for existence.
    -> TypedConstraint
    
  -- | Requires that no instance matching the 'TypedStateRef' exists on-chain.
  MustNotExist 
    :: TypedStateRef st          -- ^ State reference that must not exist.
    -> TypedConstraint
    
  -- ** Counting constraints (Currently informational, not fully enforced on-chain)

  -- | Requires exactly 'Nat' instances matching the 'TypedStateRef' to exist.
  ExactlyN 
    :: Nat                       -- ^ The exact required count.
    -> TypedStateRef st          -- ^ State reference with predicates for matching.
    -> TypedConstraint
    
  -- | Requires at most 'Nat' instances matching the 'TypedStateRef' to exist.
  AtMostN 
    :: Nat                       -- ^ The minimum required count.
    -> TypedStateRef st          -- ^ State reference with predicates for matching.
    -> TypedConstraint
    
  -- | Requires at least 'Nat' instances matching the 'TypedStateRef' to exist.
  AtLeastN 
    :: Nat                       -- ^ Minimum count
    -> TypedStateRef st          -- ^ State reference with predicates for matching.
    -> TypedConstraint
    
  -- ** Parameter-based constraints

  -- | Requires the transaction to spend a specific UTxO provided as an action parameter ('Symbol').
  -- Parameter type must be 'TxOutRef'.
  MustSpendParam -- DEPRECATED: Use MustSpendActionParam or MustSpendValidatorParam
    :: Symbol                    -- ^ Parameter name holding the 'TxOutRef' to spend.
    -> TypedConstraint
    
  -- | Requires the transaction to be signed by a key whose 'PubKeyHash' is provided
  -- as an action parameter ('Symbol'). Parameter type must be 'PubKeyHash'.
  MustBeSignedByParam 
    :: Symbol                    -- ^ Parameter name holding the required signer's 'PubKeyHash'.
    -> TypedConstraint
    
  -- | Requires the transaction to spend a specific UTxO provided as an action parameter ('Symbol').
  -- Parameter type must be 'TxOutRef'.
  MustSpendActionParam
    :: Symbol                    -- ^ Action parameter name holding the 'TxOutRef'.
    -> TypedConstraint

  -- ** Validator parameter constraints
  
  -- | Requires the transaction to spend a specific UTxO provided as a validator instance
  -- parameter (identified by validator name 'Symbol' and parameter name 'Symbol').
  -- Parameter type must be 'TxOutRef'.
  MustSpendValidatorParam 
    :: Symbol                    -- ^ Validator name ('ValidatorAppName').
    -> Symbol                    -- ^ Parameter name within the validator's 'Params'.
    -> TypedConstraint
    
  -- | Requires the transaction to be signed by a key whose 'PubKeyHash' is provided
  -- as a validator instance parameter.
  MustBeSignedByValidatorParam 
    :: Symbol                    -- ^ Validator name ('ValidatorAppName').
    -> Symbol                    -- ^ Parameter name holding the 'PubKeyHash'.
    -> TypedConstraint

  -- ** Payment constraints (for Aggregate States)

  -- | Requires the transaction to add a specific 'TypedValue' to an aggregate state ('StateType').
  -- Ensures funds are correctly deposited into the aggregate state's validator address.
  MustAddToAggregateState
    :: StateType                 -- ^ The aggregate 'StateType' (must have 'AggregateAsset' identifier).
    -> TypedValue                -- ^ The value (amount and asset) to be added.
    -> TypedConstraint

  -- | Requires the transaction to withdraw a specific 'TypedValue' from an aggregate state ('StateType')
  -- and send it to a specified destination address ('TypedValue'). Ensures funds are correctly withdrawn.
  MustWithdrawFromAggregateState
    :: StateType                 -- ^ The aggregate 'StateType'.
    -> TypedValue                -- ^ The value (amount and asset) to be withdrawn.
    -> TypedValue                -- ^ The destination address ('Address').
    -> TypedConstraint

-- | Constraints applied to the collection itself within a 'Map' operation.
data CollectionConstraint where
  -- | Ensures a specific field ('Symbol') has unique values across all items processed in the 'Map'.
  MustHaveUniqueField :: Symbol -> CollectionConstraint

-- ============================================================================
-- 6. PARAMETER DERIVATIONS
-- ============================================================================

-- | Defines a rule for deriving a named value ('Symbol') based on properties of the application instance, using a 'DerivationSource'.
-- Derived values can be used in action specifications via 'ParamValue'.
data ParamDerivation where
  -- | Derives a parameter ('Symbol') using a specified 'DerivationSource'.
  DeriveParam 
    :: Symbol                    -- ^ The name given to the derived value, usable with 'ParamValue'.
    -> DerivationSource t        -- ^ Specifies how the value of type @t@ is derived.
    -> ParamDerivation

-- | Defines sources from which validator parameters can be automatically derived.
-- The @t@ parameter indicates the type of the derived value (e.g., 'Address', 'ScriptHash').
data DerivationSource (t :: Type) where
  -- | Derives an 'Address' from another validator's script address ('Symbol' identifies the source validator).
  ValidatorAddress 
    :: Symbol                    -- ^ Source validator's 'ValidatorAppName'.
    -> DerivationSource Address
    
  -- | Derives a 'ScriptHash' from another validator's script hash ('Symbol' identifies the source validator).
  ValidatorHash 
    :: Symbol                    -- ^ Source validator's 'ValidatorAppName'.
    -> DerivationSource ScriptHash

-- ============================================================================
-- 7. GENERIC GADTS & UTILITY CLASSES
-- ============================================================================

-- | Type family to extract the selector name from GHC Generics metadata.
type family MetaSelName (sel :: Meta) :: Symbol where
  MetaSelName ('MetaSel ('Just name) _ _ _) = name

-- | Generic class for extracting a field's value ('SomeFieldValue') by name ('Text') from a record's 'Rep'.
class GExtractField (rep :: Type -> Type) where
  -- | Attempts to extract the field value. Returns 'Nothing' if the field doesn't exist.
  gExtractField :: Text -> rep p -> Maybe SomeFieldValue

-- Instances for GExtractField to traverse the generic representation...
instance {-# OVERLAPPABLE #-} GExtractField f => GExtractField (M1 i c f) where
  gExtractField fieldName (M1 x) = gExtractField fieldName x

-- Handle product types - check both sides
instance (GExtractField a, GExtractField b) => GExtractField (a :*: b) where
  gExtractField fieldName (a :*: b) = 
    gExtractField fieldName a <|> gExtractField fieldName b

-- Extract from individual fields
instance {-# OVERLAPPING #-} forall sel a.
  ( KnownSymbol (MetaSelName sel)
  , Typeable a
  , Eq a
  , ToData a
  ) => GExtractField (M1 S sel (Rec0 a)) where
  gExtractField targetField (M1 (K1 value)) = 
    let fieldName = pack $ symbolVal (Proxy @(MetaSelName sel))
    in if fieldName == targetField
       then Just (SomeFieldValue value)
       else Nothing

-- Handle unit
instance GExtractField U1 where
  gExtractField _ _ = Nothing

-- | Existential type wrapper holding a value along with 'Typeable', 'Eq', and 'ToData' constraints.
-- Allows storing or passing values heterogeneously while retaining essential capabilities.
data SomeFieldValue where
  SomeFieldValue :: (Typeable a, Eq a, ToData a) => a -> SomeFieldValue

-- Show instance focuses on indicating the presence of a value, not its content.
instance Show SomeFieldValue where
  show (SomeFieldValue _) = "SomeFieldValue <...>"

-- Eq instance uses PlutusTx's 'toBuiltinData' for comparison, handling different types safely.
instance Eq SomeFieldValue where
  (SomeFieldValue v1) == (SomeFieldValue v2) = toBuiltinData v1 == toBuiltinData v2

-- | Existential type wrapper combining a state type singleton ('SStateType') with an on-chain UTxO ('GYUTxO').
-- Includes 'StateRepresentable' and 'GExtractField' constraints needed for processing the state.
data SomeStatedUTxO where
  SomeStatedUTxO :: ( StateRepresentable st
                    , GExtractField (Rep (GetStateData st))
                    ) => SStateType st -> GYUTxO -> SomeStatedUTxO

-- Show instance provides basic identification.
instance Show SomeStatedUTxO where
  show (SomeStatedUTxO st utxo) = "SomeStatedUTxO " ++ show st ++ " " ++ show utxo
