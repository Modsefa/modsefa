---
title: "Validator Spec API"
description: "Defining the properties of an on-chain validator."
order: 3
---

# ValidatorSpec API Reference

The `ValidatorSpec` is a type-level description of a single on-chain validator. While an `AppSpec` defines the application as a whole, the `ValidatorSpec` focuses on the specific responsibilities of one validator script. This includes declaring what on-chain states it is responsible for, what parameters it requires for instantiation, and its general behavior.

## Definition

The `ValidatorSpec` is a Haskell typeclass that you will implement for a type representing your validator.

```haskell
-- From Modsefa.Core.Foundation.Types
class (KnownSymbol (ValidatorAppName v)) => ValidatorSpec (v :: Type) where
  -- | Parameter types required by this validator
  type Params v :: [(Symbol, Type)]

  -- | State types managed by this validator
  type ManagedStates v :: [StateType]

  -- | Symbolic name of this validator
  type ValidatorAppName v :: Symbol

  -- | Plutus version for this validator's scripts
  type ValidatorPlutusVersion v :: PlutusVersion

  -- | Whether this validator manages single or multiple instances
  type ValidatorInstanceType v :: InstanceType
```

## Associated Types

Each associated type in the `ValidatorSpec` defines a key property of the validator.

### `Params`

This defines the parameters required to initialize a specific instance of the validator. These are the values that will be "baked into" the validator script address.

- **Type:** `[(Symbol, Type)]` - A list of tuples, where each tuple contains the parameter's name and its Haskell type
- **Purpose:** To define the configuration parameters for the validator. This is how you create unique validator instances on-chain

**Example:**

```haskell
-- This validator is parameterized by a TxOutRef named "bootstrapUtxo".
type Params FeedValidator =
  '[ '("bootstrapUtxo", TxOutRef)
   ]
```

### `ManagedStates`

This declares the complete list of `StateType`s that this validator is responsible for managing. The Modsefa framework ensures at compile-time that any action attempting to operate on one of these states is correctly routed to this validator.

- **Type:** `[StateType]` - A list of `StateType` definitions
- **Purpose:** To explicitly associate on-chain state representations with the validator that governs their logic

**Example:**

```haskell
-- The FeedValidator manages both the configuration and the data entries for a feed.
type ManagedStates FeedValidator =
  '[ FeedConfigState
   , FeedDataState
   ]
```

### `ValidatorAppName`

This provides a unique, type-level name for the validator. This name is used in the `AppSpec` to refer to this validator.

- **Type:** `Symbol` - A type-level string
- **Purpose:** To provide a unique identifier for the validator within the application's type-level specification

**Example:**

```haskell
type ValidatorAppName FeedValidator = "FeedValidator"
```

### `ValidatorPlutusVersion`

This specifies which version of the Plutus script language the validator will be compiled to.

- **Type:** `PlutusVersion` - A type from Genius Yield (e.g., `PlutusV2`, `PlutusV3`)
- **Purpose:** To ensure the correct compiler settings and script features are used for the validator

```haskell
type ValidatorPlutusVersion FeedValidator = PlutusV3
```

### `ValidatorInstanceType`

This specifies the validator's role within the application architecture—whether the application is designed to have only one of this kind of validator, or many.

- **Type:** `InstanceType` - An enum with two variants: `'SingleInstance` and `'MultiInstance`
- **Purpose:**  To declare the intended architectural role of the validator within the dApp

**Example:**

```haskell
type ValidatorInstanceType FeedValidator = SingleInstance
```

## Complete Example

Here is a complete, well-documented `ValidatorSpec` for the `FeedValidator`.

```haskell
-- From Modsefa.Examples.Feed.Validators

-- A simple data type to represent our validator
data FeedValidator

instance ValidatorSpec FeedValidator where
  -- The name used to refer to this validator in the AppSpec.
  type ValidatorAppName FeedValidator = "FeedValidator"

  -- The validator scripts will be compiled to Plutus V3.
  type ValidatorPlutusVersion FeedValidator = PlutusV3

  -- This validator is parameterized by a TxOutRef that is referred to
  -- by the name "bootstrapUtxo" throughout the application spec.
  type Params FeedValidator =
    '[ '("bootstrapUtxo", TxOutRef)
     ]

  -- This validator is responsible for managing two types of on-chain state:
  -- the feed's configuration and its data entries.
  type ManagedStates FeedValidator =
    '[ FeedConfigState
     , FeedDataState
     ]

  -- The FeedApp is designed to contain only one instance of the FeedValidator
  type ValidatorInstanceType FeedValidator = SingleInstance
```