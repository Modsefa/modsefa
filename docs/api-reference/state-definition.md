---
title: "State Definition API"
description: "Defining on-chain state representations."
order: 4
---

# State Definition API Reference

In Modsefa, on-chain states are not just raw data; they are strongly-typed representations that declare their structure, how they are identified on the ledger, and how they can be referenced in actions. This document covers the core components for defining these states.

## `StateType`

The `StateType` is the most fundamental building block for state definition. It's a type-level construct that links a symbolic name to the actual Haskell data type that holds the on-chain data (the datum).

### Definition

```haskell
-- From Modsefa.Core.Foundation.Types
data StateType = ST Symbol Type
```

- `Symbol`: A type-level string that provides a descriptive name for the state
Type: The Haskell record type that defines the fields of the state (e.g., FeedConfig)
- `Type`: The Haskell record type that defines the fields of the state (e.g., `FeedConfig`)

**Example:**

```haskell
-- Defines a state named "FeedConfig" represented by the FeedConfig record.
type FeedConfigState = 'ST "FeedConfig" FeedConfig
```

This `FeedConfigState` type can now be used in `ValidatorSpec`s and `ActionSpec`s to refer to this specific kind of on-chain state.

---

## `StateRepresentable`

While `StateType` defines the "what", the `StateRepresentable` typeclass defines the "how". It specifies how a `StateType` is managed on the blockchain, including its on-chain identification and allowed referencing patterns. Every `StateType` used in a `ValidatorSpec` must have an instance of this class.

### Definition

```haskell
-- From Modsefa.Core.Foundation.Types
class ( ... ) => StateRepresentable (st :: StateType) where
  -- | How this state type is identified on-chain
  stateIdentifier :: proxy st -> StateIdentifier

  -- | What reference strategies are allowed for this state type
  type AllowedRefStrategy st :: RefStrategy
  type AllowedRefStrategy st = 'AnyRef -- Default strategy
```

### `stateIdentifier`

This function defines how UTxOs representing this state are identified on the ledger.

- **Type:** `StateIdentifier`
- **Purpose:** To declare the on-chain identification method for instances of this state type

**`StateIdentifier` Variants:**

- `'TokenIdentified PolicySource GYTokenName Integer`: The UTxO is identified by containing a specific quantity of a given token
  - `PolicySource`: Defines the origin of the identifying token's minting policy. It can be `'OwnPolicy` (the minting policy is the same as the validator script) or `'ExternalPolicy CurrencySymbol` (the token is minted by a different, specified policy)
  - `GYTokenName`: The name of the identifying token
  - `Integer`: The required quantity of the token that must be present in the UTxO
- `'AggregateAsset GYAssetClass`: The "state" is the total quantity of a specific asset at the validator address. This is used for treasury-like patterns
- `'NoToken`: The state is not identified by a token

### `AllowedRefStrategy`

This associated type declares how this state is allowed to be looked up or referred to within an action's `operations`.

- **Type:** `RefStrategy`
- **Purpose:** To enforce constraints on how developers can query for this state, preventing invalid or ambiguous lookups

** `RefStrategy` Variants:**

- `'OnlyByProperty`: The state can only be referenced by querying for its properties (e.g., `'TypedUniqueWhere`). It cannot be referenced as a singleton
- `'OnlyAsUnique`: The state can only be referenced as the single unique instance (via `'TypedTheOnlyInstance`). This is for states that are guaranteed to be singletons
- `'AnyRef` (Default): The state can be referenced by any valid method
- `'NoRef`: The state cannot be directly referenced in operations. This is used for aggregate states like treasuries, which are manipulated via special constraints (`'MustAddToAggregateState`) rather than direct Update operations

---

## Complete Example

Here is the complete definition for the `FeedConfigState` from the feed application example.

```haskell
-- From Modsefa.Examples.Feed.Types

-- 1. Define the Haskell record for the datum
data FeedConfig = FeedConfig
  { feedName  :: BuiltinByteString
  , feedOwner :: PubKeyHash
  }
-- ... (Generic, ToData, FromData, etc. instances) ...

-- 2. Define the StateType
type FeedConfigState = 'ST "FeedConfig" FeedConfig

-- 3. Implement the StateRepresentable instance
instance StateRepresentable FeedConfigState where
  -- This state is identified by a token minted by the validator's own policy.
  stateIdentifier _ = TokenIdentified OwnPolicy "FeedConfig" 1

  -- This state is a singleton and must only be referenced as such.
  type AllowedRefStrategy FeedConfigState = 'OnlyAsUnique
```