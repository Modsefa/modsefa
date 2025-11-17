---
title: "State Definition API"
description: "Defining on-chain state representations."
order: 4
---

# State Definition API Reference

In Modsefa, on-chain states are not just raw data; they are strongly-typed representations that declare their structure, how they are identified on the ledger, and how they can be referenced in actions. This document covers the core components for defining these states.

## `StateSpec`

The `StateSpec` is the unified typeclass for defining all properties of an on-chain state. You implement this for a tag type (e.g., `data MyStateTag`), and Modsefa uses the instance to generate the actual datum type (e.g., `data MyState`) along with all its necessary on-chain instances (`ToData`, `FromData`, `makeLift`, etc.) using the `$(generateStateDatum)` and `$(generateStateInstances)` splices.

```haskell
-- From Modsefa.Core.Foundation.Types
class ( KnownSymbol (DatumName s)
      , Typeable s
      , ValidateStateSpec s
      ) => StateSpec (s :: Type) where
  -- | The symbolic name for the datum type to be generated.
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
```

### Associated Types

- `DatumName`: The name (as a `Symbol`) for the Haskell record type that will be generated (e.g., `"FeedConfig"`).
- `DatumFields`: A type-level list of `(Symbol, Type)` tuples defining the field names and types for the generated datum record.
- `Identifier`: A `SpecStateIdentifier` (e.g., `'TokenIdentifiedSpec 'OwnPolicySpec "FeedConfig" 1`) defining how UTxOs of this state are found on-chain.
- `Strategy`: A `RefStrategy` (e.g., `'OnlyAsUnique`, `'OnlyByProperty`) defining how this state can be referenced in actions.
- `HasMappable`: A `Bool` indicating if the generated datum type should have a `Mappable` instance, which is required for use in `'Map'` (batch) operations.

---

## Complete Example

Here is the complete definition for the `FeedConfigState` from the feed application example.

```haskell
-- From Modsefa.Examples.Feed.Types

-- 1. Define an empty data type to act as the state tag 
data FeedConfigState

-- 2. Implement the StateSpec instance for the tag
instance StateSpec FeedConfigState where 
  -- This will generate a 'data FeedConfig = ...' 
  type type DatumName FeedConfigState = "FeedConfig"

  -- These will be the record fields in the generated 'FeedConfig' 
  type type DatumFields FeedConfigState = 
    '[ '("feedName", BuiltinByteString)
     , '("feedOwner", PubKeyHash) 
     ]

  -- This state is identified by a "FeedConfig" token
  type Identifier FeedConfigState = 'TokenIdentifiedSpec 'OwnPolicySpec "FeedConfig" 1

  -- This state is a singleton and must only be referenced as such
  type Strategy FeedConfigState = 'OnlyAsUnique
```

You would then use `$(generateStateDatum @FeedConfigState)` in a separate module (e.g., `Generated/Datums.hs`) to automatically generate the `FeedConfig` data type, followed by `$(generateStateInstances)` to create its Plutus instances.