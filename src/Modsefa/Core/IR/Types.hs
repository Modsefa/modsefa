{-|
Module      : Modsefa.Core.IR.Types
Description : Defines the Intermediate Representation (IR) for Modsefa applications.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module defines the data types that constitute the Modsefa Intermediate
Representation (IR). The IR is a simplified, value-level representation of a
smart contract application, compiled from the type-level 'Modsefa.Core.Foundation.AppSpec'
via the singletons defined in 'Modsefa.Core.Singletons.Types'.

The IR serves as a backend-agnostic interface suitable for analysis, optimization,
and code generation (e.g., generating Plutus Tx validator scripts). It abstracts
away the complexities of the type-level specification and GADTs used in the
foundation and singleton layers.
-}
-- | Defines the Intermediate Representation (IR) for a Modsefa application.
--
-- The IR is a simplified, value-level representation of the entire smart
-- contract application, compiled from the type-level `AppSpec`. It serves
-- as a clean, backend-agnostic interface for analysis, optimization, and
-- code generation.
module Modsefa.Core.IR.Types
  ( ActionIR(..)
  , AppIR(..)
  , StateInfoIR(..)
  , BatchOperationIR(..)
  , CollectionConstraintIR(..)
  , ConstraintIR(..)
  , FieldValueIR(..)
  , InstanceCheckIR(..)
  , OperationIR(..)
  , PubKeyHashIR(..)
  , PolicySourceIR(..)
  , DatumFieldIR(..)
  , ValidatorIR(..)
  ) where

import Data.Text (Text)

import GeniusYield.Types (PlutusVersion)

import Modsefa.Core.Foundation.Types (SomeFieldValue)


-- | The root of the IR, representing the entire application structure.
data AppIR = AppIR
  { appIRName      :: Text -- ^ The name of the application.
  , appIRValidators :: [ValidatorIR] -- ^ List of validators comprising the application.
  , appIRStates     :: [StateInfoIR] -- ^ Registry of all states used in the application.
  } deriving (Show, Eq)

-- | IR representation of a state type's metadata.
-- Maps the unique state tag to its underlying on-chain datum type.
data StateInfoIR = StateInfoIR
  { stateInfoName :: Text -- ^ The unique state tag name (e.g., "FeedDataState").
  , stateInfoDatumName :: Text -- ^ The name of the on-chain datum type (e.g., "FeedData").
  , stateInfoTokenName :: Text -- ^ The token name associated with this state's identifying asset.
  , stateInfoPolicy :: PolicySourceIR -- ^ The policy source for this state's identifying asset.
  , stateInfoMappable :: Bool -- ^ Indicates if the state can be used in Map operations.
  , stateInfoFields :: [DatumFieldIR] -- ^ Fields for the datum type of the state.
  } deriving (Show, Eq)

-- | IR representation of a single validator script and its associated logic.
data ValidatorIR = ValidatorIR
  { validatorIRName   :: Text -- ^ The unique name of the validator (from 'Modsefa.Core.Foundation.ValidatorAppName').
  , validatorIRParams :: [(Text, SomeFieldValue)] -- ^ Resolved instance parameters specific to this validator instance.
  , validatorIRVersion :: PlutusVersion -- ^ The Plutus version targeted by this validator.
  , validatorIRManagedStates :: [Text] -- ^ List of state names ('Modsefa.Core.Foundation.GetStateName') managed by this validator.
  , validatorIRActions :: [ActionIR] -- ^ List of actions this validator can handle (derived from 'Modsefa.Core.Foundation.ActionTransitions').
  } deriving (Show, Eq)

-- | IR representation of a single action that a validator can perform.
-- Corresponds to a specific redeemer value for the validator script.
data ActionIR = ActionIR
  { actionIRName        :: Text -- ^ The name of the action (from 'Modsefa.Core.Foundation.ActionSpecName'), used as the redeemer.
  , actionIROperations  :: [OperationIR] -- ^ List of operations performed by this action.
  , actionIRConstraints :: [ConstraintIR] -- ^ List of constraints enforced by this action.
  } deriving (Show, Eq)

-- | IR representation of batch operations performed within a 'Modsefa.Core.Foundation.Map' step.
data BatchOperationIR
  = BatchCreateIR
      { batchOpIRStateName :: Text -- ^ The name of the state being created in batch.
      , batchOpIRFields    :: [(Text, FieldValueIR)] -- ^ Field assignments for each created instance.
      }
  | BatchDeleteIR
      { batchOpIRStateName :: Text -- ^ The name of the state being deleted in batch.
      }
  deriving (Show, Eq)

-- | IR representation of a fundamental operation on a state instance.
data OperationIR
  = CreateOpIR -- ^ Corresponds to 'Modsefa.Core.Foundation.Create'.
      { opIRStateName :: Text -- ^ Name of the state being created.
      , opIRFields    :: [(Text, FieldValueIR)] -- ^ Field assignments (field name, value source).
      }
  | UpdateOpIR -- ^ Corresponds to 'Modsefa.Core.Foundation.Update'.
      { opIRStateName :: Text -- ^ Name of the state being updated.
      , opIRFields    :: [(Text, FieldValueIR)] -- ^ Field modifications (includes 'FromInputField' for preserved fields).
      }
  | DeleteOpIR -- ^ Corresponds to 'Modsefa.Core.Foundation.Delete'.
      { opIRStateName :: Text -- ^ Name of the state being deleted.
      }
  | ReferenceOpIR -- ^ Corresponds to 'Modsefa.Core.Foundation.Reference', typically within a 'Modsefa.Core.Foundation.Let'.
      { opIRStateName :: Text -- ^ Name of the state being referenced.
      , opIRLabel     :: Text -- ^ The label assigned to this reference via 'Modsefa.Core.Foundation.Let'.
      }
  | BatchOpIR -- ^ Corresponds to 'Modsefa.Core.Foundation.Map'.
      { opIRBatchOperation  :: BatchOperationIR -- ^ The specific batch operation (Create or Delete).
      , opIRCollectionParam :: Text -- ^ The name of the action parameter (list) being mapped over.
      , opIRConstraints     :: [CollectionConstraintIR] -- ^ Constraints applied to the collection.
      }
  deriving (Show, Eq)

-- | IR representation of the source or computation of a field's value.
-- Mirrors 'Modsefa.Core.Foundation.TypedValue' but in a simpler, value-level format.
data FieldValueIR
  = FromActionParam Text -- ^ Value comes from an action parameter ('Modsefa.Core.Foundation.ParamValue'). Text is parameter name.
  | FromEnum Text Text -- ^ Value is an enum constructor ('Modsefa.Core.Foundation.EnumValue'). Texts are Type name, Constructor name.
  | FromInt Integer -- ^ Value is a literal integer ('Modsefa.Core.Foundation.IntValue').
  | FromInputField Text -- ^ Value is preserved from the input datum's field during an update ('Modsefa.Core.Foundation.Preserve'). Text is field name.
  | FromStateField Text Text -- ^ Value comes from a field of a referenced state ('Modsefa.Core.Foundation.StateFieldValue'). Texts are Let label, Field name.
  | CurrentTimeIR -- ^ Value is the transaction's validity start time ('Modsefa.Core.Foundation.CurrentTime').
  | AddValueIR FieldValueIR FieldValueIR -- ^ Arithmetic Add ('Modsefa.Core.Foundation.AddValue').
  | SubtractValueIR FieldValueIR FieldValueIR -- ^ Arithmetic Subtract ('Modsefa.Core.Foundation.SubtractValue').
  | MultiplyValueIR FieldValueIR FieldValueIR -- ^ Arithmetic Multiply ('Modsefa.Core.Foundation.MultiplyValue').
  | DivideValueIR FieldValueIR FieldValueIR -- ^ Arithmetic Divide ('Modsefa.Core.Foundation.DivideValue').
  deriving (Show, Eq)

-- | IR representation of a transaction constraint that must be validated on-chain.
-- Mirrors relevant constructors from 'Modsefa.Core.Foundation.TypedConstraint'.
data ConstraintIR
  = MustBeSignedBy PubKeyHashIR -- ^ Corresponds to 'Modsefa.Core.Foundation.MustBeSignedByParam' or 'Modsefa.Core.Foundation.MustBeSignedByState'.
  | MustSpendActionParamIR Text -- ^ Corresponds to 'Modsefa.Core.Foundation.MustSpendActionParam'. Text is parameter name.
  | MustAddToAggregateStateIR Text FieldValueIR  -- ^ Corresponds to 'Modsefa.Core.Foundation.MustAddToAggregateState'. Text is state name.
  | MustWithdrawFromAggregateStateIR Text FieldValueIR FieldValueIR -- ^ Corresponds to 'Modsefa.Core.Foundation.MustWithdrawFromAggregateState'. Text is state name. Value, Destination Address.
  | MustCheckInstance InstanceCheckIR -- ^ Represents an instance consistency check derived during compilation.
  deriving (Show, Eq)

-- | IR representation of a 'Modsefa.Core.Foundation.CollectionConstraint'.
newtype CollectionConstraintIR
  = MustHaveUniqueFieldIR Text -- ^ Corresponds to 'Modsefa.Core.Foundation.MustHaveUniqueField'. Text is Field Name.
  deriving (Show, Eq)

-- | IR representation specifying the source of a required 'PubKeyHash' for signing constraints.
data PubKeyHashIR
  = FromActionParamPKH Text -- ^ PKH comes from an action parameter ('Modsefa.Core.Foundation.MustBeSignedByParam'). Text is parameter name.
  | FromStateFieldPKH Text Text -- ^ PKH comes from a field of a referenced state ('Modsefa.Core.Foundation.MustBeSignedByState'). Texts are State Name, Field Name.
  deriving (Show, Eq)

-- | IR representation of the policy source for a state's identifier.
data PolicySourceIR
  = OwnPolicyIR -- ^ The asset is minted by the validator associated with the state the asset represents.
  | ExternalPolicyIR Text -- ^ The asset is from an external policy, identified by its generic CurrencySymbol (hex-encoded).
  deriving (Show, Eq)

-- | Represents a single field in a state's datum.
data DatumFieldIR = DatumFieldIR
  { datumFieldName :: Text -- ^ The name of the field.
  , datumFieldType :: Text -- ^ The Haskell type of the field as a string (e.g., "Integer", "Maybe PubKeyHash").
  } deriving (Show, Eq)

-- | IR representation of an instance consistency check constraint.
-- These are generated during IR compilation to ensure references across validator boundaries are valid.
data InstanceCheckIR
  -- | Check used within the *same* validator script instance. Verifies that the address of the referenced state
  -- matches the address of the input currently being spent by this script instance.
  = SameAddressAsReferenceIR
      { scarReferenceState :: Text -- ^ State name ('Modsefa.Core.Foundation.GetStateName') providing the expected address.
      }
  -- | Check used within a *derived* validator script instance.
  -- Verifies that the address of the referenced state matches the address provided as a parameter to the current validator script.
  | AddressMatchesParamIR
      { amprReferenceState :: Text -- ^ State name providing the expected address.
      , amprParamName      :: Text -- ^ Name of the 'Address' parameter in the current derived validator's 'Params'.
      }
  deriving (Show, Eq)