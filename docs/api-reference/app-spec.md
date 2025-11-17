---
title: "App Spec API"
description: "Defining the top-level application specification."
order: 2
---

# AppSpec API Reference

The `AppSpec` serves as the blueprint for a Modsefa application, bringing together all its individual components like validators, states, and actions into a single, cohesive specification. This is the main entry point for the Modsefa framework to understand your application's logic and generate the necessary on-chain and off-chain code.

## Definition

The `AppSpec` is defined as a Haskell typeclass that your application's main type will implement.

```haskell
-- From Modsefa.Core.Foundation.Types
class
  ( InitialStateInStates app
  , ValidateAppInstanceParameters app
  , ValidateActionTransitions (ActionTransitions app) app
  ) => AppSpec (app :: Type) where

  -- | List of validators in this application
  type Validators app :: [ValidatorDef]

  -- | Application-level state names (for state machine)
  type AppStates app :: [Symbol]

  -- | Initial application state
  type InitialAppState app :: Symbol

  -- | Valid action transitions with their state changes
  type ActionTransitions app :: [(TypedActionSpec, Symbol, Symbol)]

  -- | Instance-specific parameters for validators
  type AppInstanceParameters app :: [(Symbol, Symbol)]

  -- | Automatic parameter derivations between validators
  type ParameterDerivations app :: [ParamDerivation]
```

## Associated Types

The power of the `AppSpec` lies in its associated types, which allow you to declare the structure and behavior of your application at the type level.

### `Validators`

This type family defines all the validators that are part of your application. Each validator is responsible for managing a specific set of on-chain states.

- **Type:** `[ValidatorDef]` - A list of `ValidatorDef`s
- **Purpose:** To register the full set of validators that comprise the application. This list is essential for the framework to perform compile-time validation of the entire spec, generate the correct on-chain scripts, and build client-side transaction tooling

**Example:**

```haskell
type Validators FeedApp =
  '[ 'Validator FeedValidator
   ]
```

### `AppStates`

This defines the possible states of the application's top-level state machine. This is useful for modeling the lifecycle of your dApp.

- **Type:** `[Symbol]` - A list of type-level strings (Symbols)
- **Purpose:** To enumerate the high-level states of the application

**Example:**

```haskell
type AppStates FeedApp = '["Uninitialized", "Initialized"]
```

### `InitialAppState`

This specifies the starting state of the application's state machine.

- **Type:** `Symbol` - A type-level string that must be one of the states defined in `AppStates`
- **Purpose:** To define the entry point of the application's lifecycle

**Example:**

```haskell
type InitialAppState FeedApp = "Uninitialized"
```

### `ActionTransitions`

This is one of the most critical parts of the `AppSpec`. It defines the valid state transitions that can occur in the application's state machine, triggered by specific actions.

- **Type**: `[(TypedActionSpec, Symbol, Symbol)]` - A list of tuples, where each tuple contains:
  1. `TypedActionSpec`: The action that triggers the transition
  2. `Symbol`: The state before the action is executed
  3.  `Symbol`: The state after the action is successfully executed
- **Purpose**: To define the application's state transition logic. This critically associates actions with the application and dictates the valid sequences of operations, forming the edges of the state machine graph

**Example:**

```haskell
type ActionTransitions FeedApp =
  '[ '(InitializeFeedSpec, "Uninitialized", "Initialized")
   , '(UpdateFeedSpec, "Initialized", "Initialized")
   ]
```

### `AppInstanceParameters`

This associated type defines the parameters required to create a unique, deployable instance of the application. An application instance is defined by specific parameters passed to its underlying validators. For example, the `FeedApp` instance is uniquely identified by the `"bootstrapUtxo"` that parameterizes its `FeedValidator`.

- **Type:** `[(Symbol, Symbol)]` - A list of tuples, where the first element is the name of a validator and the second is the name of a parameter that validator requires
- **Purpose:** To declare the full set of parameters that, together, define a unique instance of the dApp

**Example:**

```haskell
type AppInstanceParameters FeedApp =
  '[ '("FeedValidator", "bootstrapUtxo")
   ]
```

### `ParameterDerivations`

This associated type allows you to define new parameters that are available throughout the application's specification. The values for these parameters are not provided manually; instead, they are automatically derived from on-chain properties (like the address or script hash) of the application's own validators. This creates a powerful way to reference a validator's identity in other parts of the dApp's definition, such as:

1. **Parameterizing Other Validators:** One validator can be parameterized by the on-chain identity of another, creating a direct link between them
2. **Informing Action Specs:** An action can reference a derived parameter, allowing it to know the address of a specific validator without that address being manually provided by the end-user

- **Type:** `[ParamDerivation]` - A list of parameter derivation definitions.
- **Purpose:** To define application-scoped parameters whose values are derived from the on-chain properties of the application's validators. This enables robust, programmatic linking between different parts of the specification, reducing manual configuration and user-supplied inputs

#### Definition

The `ParamDerivation` type links a parameter name to a `DerivationSource`.

```haskell
-- From Modsefa.Core.Foundation.Types
data ParamDerivation where
  DeriveParam
    :: Symbol                    -- ^ Parameter name to derive
    -> DerivationSource t        -- ^ Source for derivation
    -> ParamDerivation
```

`DerivationSource`

The `DerivationSource` type specifies where the parameter's value should come from.

```haskell
-- From Modsefa.Core.Foundation.Types
data DerivationSource (t :: Type) where
  -- | Derive from another validator's script address
  ValidatorAddress
    :: Symbol                    -- ^ Source validator name
    -> DerivationSource Address

  -- | Derive from another validator's hash
  ValidatorHash
    :: Symbol                    -- ^ Source validator name
    -> DerivationSource ScriptHash
```

**Source Types:**
- `'ValidatorAddress "validatorName"`: Derives a parameter of type `Address` from the script address of the validator named `"validatorName"`

- `'ValidatorHash "validatorName"`: Derives a parameter of type `ScriptHash` from the script hash of the validator named `"validatorName"`

**Example:**

```haskell
type ParameterDerivations SubscriptionApp =
  '[ 'DeriveParam "serviceAddress"
       ('ValidatorAddress "ServiceAndPricingValidator")
   ]
```

## Complete Example

Here is the complete `AppSpec` for the `FeedApp` example, which brings all the pieces together.

```haskell
-- From Modsefa.Examples.Feed.Spec

data FeedApp

instance AppSpec FeedApp where
  -- The application uses a single validator, the FeedValidator.
  type Validators FeedApp =
    '[ 'Validator FeedValidator
     ]

  -- The application has two main states: before and after initialization.
  type AppStates FeedApp = '["Uninitialized", "Initialized"]

  -- It starts in the "Uninitialized" state.
  type InitialAppState FeedApp = "Uninitialized"

  -- Defines the actions that drive the state machine.
  type ActionTransitions FeedApp =
    '[ '(InitializeFeedSpec, "Uninitialized", "Initialized") -- InitializeFeed transitions from Uninitialized to Initialized
     , '(UpdateFeedSpec, "Initialized", "Initialized")       -- UpdateFeed can only be called when Initialized, and stays Initialized
     ]

  -- The FeedValidator requires a "bootstrapUtxo" parameter for its instantiation.
  type AppInstanceParameters FeedApp =
    '[ '("FeedValidator", "bootstrapUtxo")
     ]

  -- This application does not have any parameter derivations between validators.
  type ParameterDerivations FeedApp = '[]
```

This `AppSpec` provides a complete, type-level blueprint of the `FeedApp`. The Modsefa framework uses this specification to generate code that enforces all the declared rules on-chain, providing a high degree of security and correctness.