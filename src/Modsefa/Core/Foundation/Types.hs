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
core definitions. It defines core concepts like 'StateSpec', 'AppSpec',
'ValidatorSpec', 'TypedActionSpec', operations ('TypedOperation'),
constraints ('TypedConstraint'), and related helpers.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
--   2. State Specification
--   3. State Representation
--   4. Action Specifications 
--   5. State References & Predicates
--   6. Constraints
--   7. Parameter Derivations
--   8. Helpers & Existential Types
--   9. Static State Validation
module Modsefa.Core.Foundation.Types
  ( -- * Core Primitives
    PolicySource(..)
  , StateIdentifier(..)
  , RefStrategy(..)
    
    -- * State Specification
  , StateSpec(..)
  , SpecPolicySource(..)
  , SpecStateIdentifier(..)

    -- * State Representation
  , StateDatum

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
    
    -- * Helpers
  , SomeStatedUTxO(..)
  , SomeFieldValue(..)
  , GExtractField(..)
  , StateDatumConstraints
  , MetaSelName

    -- * Type Aliases
  , DerivationContext
  , AdaPolicy
  , AdaTokenName
  ) where

import Control.Applicative ((<|>))
import Data.Kind (Constraint, Type)
import Data.Map (Map)
import Data.Text (Text, null, pack)
import Data.Typeable (Proxy(Proxy), Typeable)
import GHC.Generics 
  ( Generic(Rep), K1(K1), M1(M1), Meta(MetaSel), Rec0, S, U1, (:*:)((:*:))
  )
import GHC.TypeLits (KnownSymbol, Nat, Symbol, symbolVal, TypeError, ErrorMessage (Text, (:<>:)))

import GeniusYield.Types (GYAssetClass, GYTokenName, GYUTxO)
import PlutusLedgerApi.V3 
  ( Address, CurrencySymbol, ScriptHash, ToData(toBuiltinData)
  )
import PlutusTx (FromData)


-- ============================================================================
-- 1. CORE PRIMITIVES
-- ============================================================================

-- | Policy source for state identification tokens.
data PolicySource
  = OwnPolicy          -- ^ Use the validator's own minting policy script hash as the 'CurrencySymbol'.
  | ExternalPolicy CurrencySymbol  -- ^ Use a specified external 'CurrencySymbol'.
  deriving (Eq, Show)

-- | Defines how distinct instances of a 'StateSpec' are identified on-chain.
data StateIdentifier
  = TokenIdentified PolicySource GYTokenName Integer  -- ^ Identified by tokens
  | AggregateAsset GYAssetClass                       -- ^ Identified by a quantity
  deriving (Eq, Show)

-- | Reference strategy allowed for a 'StateSpec'.
-- Determines how instances of this state can be looked up or referenced in operations.
data RefStrategy 
  = OnlyByProperty   -- ^ Can only reference by property queries
  | OnlyAsUnique     -- ^ Can only reference as unique instance
  | AnyRef           -- ^ Can reference by any method
  | NoRef            -- ^ Cannot be referenced in operations (aggregated-based states)
  deriving (Eq, Show)

-- ============================================================================
-- 2. STATE SPECIFICATION
-- ============================================================================

-- | Type-level equivalent of 'PolicySource'.
data SpecPolicySource
  = OwnPolicySpec
  | ExternalPolicySpec Symbol

-- | Type-level equivalent of 'StateIdentifier'.
data SpecStateIdentifier
  = TokenIdentifiedSpec SpecPolicySource Symbol Nat
  | AggregateAssetSpec SpecPolicySource Symbol
  | NoTokenSpec

-- | Standard policy for ADA (Lovelace).
-- In Plutus, ADA is represented by an empty CurrencySymbol.
type AdaPolicy = 'ExternalPolicySpec ""

-- | Standard token name for ADA (Lovelace).
-- In Plutus, ADA is represented by an empty TokenName.
type AdaTokenName = ""

-- | The core specification class for a state type, defining its properties strictly at the type level.
-- | An instance of this class for a type 's' (e.g., 'FeedConfigState') provides all
-- | the metadata Modsefa needs to generate its on-chain datum type and validation logic.
class ( KnownSymbol (DatumName s)
      , Typeable s
      , ValidateStateSpec s
      ) => StateSpec (s :: Type) where
  -- | The symbolic name for this state.
  type DatumName s :: Symbol

  -- | A type-level list defining the fields of the datum record.
  -- Format: '[ '(fieldName, fieldType) ]
  type DatumFields s :: [(Symbol, Type)]

  -- | The on-chain identification strategy for this state.
  type Identifier s :: SpecStateIdentifier

  -- | The allowed reference strategy for this state.
  type Strategy s :: RefStrategy

  -- | Whether the generated datum type should have a 'Mappable' instance.
  type HasMappable s :: Bool
  type HasMappable s = 'False

-- ============================================================================
-- 3. STATE REPRESENTATION
-- ============================================================================

-- | Open type family linking a state tag 's' to its actual on-chain data record.
-- | This is populated by the 'generateStateInstances' splice based on 'DatumName s'
type family StateDatum (s :: Type) :: Type

-- ============================================================================
-- 4. ACTION SPECIFICATIONS
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

-- | Complete action specification, linking a name, steps, constraints, and parameters.
data TypedActionSpec where
  ActionSpec 
    :: Symbol                    -- ^ The unique name identifying this action.
    -> [ActionStep]              -- ^ A sequence of 'ActionStep's defining the action's logic.
    -> [TypedConstraint]         -- ^ Validation constraints applied to the overall transaction for this action.
    -> [(Symbol, Type)]          -- ^ Parameter specifications (name and type) required by this action.
    -> TypedActionSpec

-- | Represents fundamental operations performed on state instances within an action.
data TypedOperation where
  -- | Creates a new instance of a state.
  Create 
    :: forall s. (s ~ s) =>      -- ^ The state type tag to create.
       [FieldSpec]               -- ^ Field assignments for the new state datum.
    -> [TypedConstraint]         -- ^ Operation-specific constraints.
    -> TypedOperation
    
  -- | Updates an existing state instance.
  Update 
    :: TypedStateRef s           -- ^ Reference to the state instance to update.
    -> [FieldSpec]               -- ^ Field modifications for the state datum.
    -> [TypedConstraint]         -- ^ Operation-specific constraints.
    -> TypedOperation
    
  -- | Deletes an existing state instance.
  Delete 
    :: TypedStateRef s           -- ^ Reference to the state instance to delete.
    -> [TypedConstraint]         -- ^ Operation-specific constraints.
    -> TypedOperation
    
  -- | References an existing state instance without modifying it.
  Reference 
    :: TypedStateRef s           -- ^ Reference to the state instance to read.
    -> [TypedConstraint]         -- ^ Operation-specific constraints.
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
-- 5. STATE REFERENCES & PREDICATES
-- ============================================================================

-- | Type-safe reference to state instances of type 's'.
data TypedStateRef (s :: Type) where
  -- | Reference to the single, unique instance of the state.
  TypedTheOnlyInstance :: TypedStateRef s
  
  -- | Reference to the unique instance matching a predicate.
  TypedUniqueWhere 
    :: TypedPredicate s          -- ^ Predicate that must match exactly one instance.
    -> TypedStateRef s
    
  -- | References any single instance of state 's'.
  TypedAny :: TypedStateRef s

  -- | Reference to any single instance matching a predicate.
  TypedAnyWhere 
    :: TypedPredicate s          -- ^ Predicate to match instances.
    -> TypedStateRef s

  -- | Reference to a state instance previously labeled in a 'Let' step.
  TypedByLabel 
    :: Symbol                    -- ^ The label of the referenced step.
    -> TypedStateRef s

-- | Type-safe predicates for querying state instances of type 's'.
data TypedPredicate (s :: Type) where
  -- | Checks if a field equals a specific value.
  FieldEquals 
    :: Symbol                    -- ^ Field name.
    -> TypedValue                -- ^ Expected value.
    -> TypedPredicate s
    
  -- | Checks if a field is greater than a specific value.
  FieldGT 
    :: Symbol                    -- ^ Field name.
    -> TypedValue                -- ^ Value to compare against.
    -> TypedPredicate s
    
  -- | Checks if a field is less than a specific value.
  FieldLT 
    :: Symbol                    -- ^ Field name.
    -> TypedValue                -- ^ Value to compare against.
    -> TypedPredicate s

  -- | Logical AND combining two predicates for the same state type.
  And
    :: TypedPredicate s          -- ^ First predicate.
    -> TypedPredicate s          -- ^ Second predicate.
    -> TypedPredicate s

-- | Type-safe path linking a record type, field name, and field type.
-- Primarily used for type-level validation in constraints like 'PreserveStateField'.
data TypedFieldPath (record :: Type) (field :: Symbol) (fieldType :: Type) where
  TypedFieldPath :: TypedFieldPath record field fieldType

-- ============================================================================
-- 6. CONSTRAINTS
-- ============================================================================

-- | Validation constraints applied to transactions generated by actions.
data TypedConstraint where
  -- ** State-based constraints

  -- | Requires a signature from a public key stored in a state field.
  MustBeSignedByState 
    :: TypedStateRef s           -- ^ State reference containing the signer's PubKeyHash.
    -> Symbol                    -- ^ Field name holding the 'PubKeyHash'.
    -> TypedConstraint
    
  -- | Ensures a specific field remains unchanged during an update.
  PreserveStateField 
    :: TypedStateRef s           -- ^ State reference being updated.
    -> TypedFieldPath record field fieldType  -- ^ Path to the field that must be preserved.
    -> TypedConstraint
    
  -- | Requires a state field to have a specific value.
  RequireStateValue 
    :: TypedStateRef s           -- ^ State reference to check.
    -> TypedFieldPath record field fieldType  -- ^ Path to the field.
    -> TypedValue                -- ^ The required value.
    -> TypedConstraint
    
  -- ** Existence constraints

  -- | Requires that at least one instance matching the 'TypedStateRef' exists on-chain.
  MustExist 
    :: TypedStateRef s           -- ^ State reference to check for existence.
    -> TypedConstraint
    
  -- | Requires that no instance matching the 'TypedStateRef' exists on-chain.
  MustNotExist 
    :: TypedStateRef s           -- ^ State reference that must not exist.
    -> TypedConstraint
    
  -- ** Counting constraints (Currently informational, not fully enforced on-chain)

  -- | Requires exactly 'Nat' instances matching the 'TypedStateRef' to exist.
  ExactlyN 
    :: Nat                       -- ^ The exact required count.
    -> TypedStateRef s           -- ^ State reference with predicates for matching.
    -> TypedConstraint
    
  -- | Requires at most 'Nat' instances matching the 'TypedStateRef' to exist.
  AtMostN 
    :: Nat                       -- ^ The minimum required count.
    -> TypedStateRef s           -- ^ State reference with predicates for matching.
    -> TypedConstraint
    
  -- | Requires at least 'Nat' instances matching the 'TypedStateRef' to exist.
  AtLeastN 
    :: Nat                       -- ^ Minimum count
    -> TypedStateRef s           -- ^ State reference with predicates for matching.
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

  -- | Requires adding value to an aggregate state (e.g., treasury).
  MustAddToAggregateState
    :: s                         -- ^ Aggregate state to add to
    -> TypedValue                -- ^ Value to add.
    -> TypedConstraint

  -- | Requires withdrawing value from an aggregate state.
  MustWithdrawFromAggregateState
    :: s                         -- ^ Aggregate state to withdraw from
    -> TypedValue                -- ^ Value to withdraw.
    -> TypedValue                -- ^ Destination address.
    -> TypedConstraint

-- | Constraints applied to the collection itself within a 'Map' operation.
data CollectionConstraint where
  -- | Ensures a specific field ('Symbol') has unique values across all items processed in the 'Map'.
  MustHaveUniqueField :: Symbol -> CollectionConstraint

-- ============================================================================
-- 7. PARAMETER DERIVATIONS
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
-- 8. GENERIC GADTS & UTILITY CLASSES
-- ============================================================================

-- | Type family to extract the selector name from GHC Generics metadata.
type family MetaSelName (sel :: Meta) :: Symbol where
  MetaSelName ('MetaSel ('Just name) _ _ _) = name
  MetaSelName ('MetaSel 'Nothing _ _ _) = ""

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
    -- Ensure we only match if the field actually HAS a name (is not empty)
    in if not (Data.Text.null fieldName) && fieldName == targetField
       then Just (SomeFieldValue value)
       else Nothing

-- Handle unit
instance GExtractField U1 where
  gExtractField _ _ = Nothing

-- | A constraint bundle for all required instances on a 'StateDatum' type.
-- |
-- | This class collects the constraints (like 'Generic', 'ToData', 'FromData', 'GExtractField')
-- | that are required by functions operating on 'StateDatum' values.
-- | It is used to simplify the type signatures of internal Modsefa functions.
class ( StateSpec s
      , Generic (StateDatum s)
      , GExtractField (Rep (StateDatum s))
      , ToData (StateDatum s)
      , FromData (StateDatum s)
      , Eq (StateDatum s)
      , Show (StateDatum s)
      , Typeable (StateDatum s)
      ) => StateDatumConstraints (s :: Type)

-- | Blanket instance to automatically satisfy 'StateDatumConstraints'
-- whenever all individual constraints are met.
instance ( StateSpec s
         , Generic (StateDatum s)
         , GExtractField (Rep (StateDatum s))
         , ToData (StateDatum s)
         , FromData (StateDatum s)
         , Eq (StateDatum s)
         , Show (StateDatum s)
         , Typeable (StateDatum s)
         ) => StateDatumConstraints s

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

-- | Type alias for the map holding resolved derived parameter values ('ParamDerivation').
-- Keys are the derived parameter names ('Symbol'), values are 'SomeFieldValue'.
type DerivationContext = Map Text SomeFieldValue

-- | Existential type wrapper combining a state proxy with an on-chain UTxO.
-- Uses 'Proxy s' instead of 'SStateSpec s' to avoid cyclic dependencies.
data SomeStatedUTxO where
  SomeStatedUTxO :: ( StateSpec s
                    , FromData (StateDatum s)
                    , ToData (StateDatum s)
                    , Generic (StateDatum s)
                    , GExtractField (Rep (StateDatum s))
                    ) => Proxy s -> GYUTxO -> SomeStatedUTxO

-- | Show instance for 'SomeStatedUTxO'.
instance Show SomeStatedUTxO where
  show (SomeStatedUTxO (_ :: Proxy s) utxo) =
    "SomeStatedUTxO @" ++ symbolVal (Proxy @(DatumName s)) ++ " " ++ show utxo

-- ============================================================================
-- 9. STATIC STATE VALIDATION
-- ============================================================================

-- | Top-level validation constraint for a single 'StateSpec'.
-- | This is hooked into by 'AllStatesHaveSpec' in Modsefa.Core.Foundation.Validator.
type family ValidateStateSpec (s :: Type) :: Constraint where
  ValidateStateSpec s =
    ( -- Check consistency of Aggregate states (Your Idea #1)
      ValidateAggregateStateSpec (Identifier s) (DatumFields s) (Strategy s)
    -- ... other static StateSpec checks can be added here ...
    )

-- | Validation for Idea #1: Checks consistency for AggregateAssetSpec
type family ValidateAggregateStateSpec (ident :: SpecStateIdentifier) (fields :: [(Symbol, Type)]) (strat :: RefStrategy) :: Constraint where
  -- If it's not AggregateAssetSpec, it's valid (by this check)
  ValidateAggregateStateSpec ('TokenIdentifiedSpec _ _ _) _ _ = ()
  ValidateAggregateStateSpec 'NoTokenSpec _ _ = ()
  -- If it IS AggregateAssetSpec, check the rules:
  -- Valid case: '[] fields and 'NoRef strategy
  ValidateAggregateStateSpec ('AggregateAssetSpec _ _) '[] 'NoRef = ()
  -- Error case: Any other combination
  ValidateAggregateStateSpec ('AggregateAssetSpec _ _) _ _ =
    TypeError ('Text "StateSpec using 'AggregateAssetSpec' MUST have:"
               ':<>: 'Text "  1. 'type DatumFields s = '[]'"
               ':<>: 'Text "  2. 'type Strategy s = 'NoRef'")