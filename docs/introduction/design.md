---
title: "Design"
description: "Modfesa Design"
order: 2
---

# How Modsefa Works: Framework Design and Architecture

## Overview

Modsefa transforms Cardano application development by providing a **single specification language** that captures your entire application logic. From this specification, the framework automatically generates all the code you need: validator scripts, transaction builders, while providing client libraries for interacting with your application.

This document explains the key concepts and architecture that makes this possible.

## The Big Picture: From Specification to Working Application

Modsefa transforms your **type-level specification** into a working Cardano application through a series of well-defined steps:

```
Application Specification (Type-Level Haskell)
    │
    ↓
Value-Level Singletons (SAppSpec, SActionSpec, etc.)
    │
    ├─────────────────────────┐
    │                         │
    ↓                         │
Intermediate Representation   │ (Used By)
(IR - AppIR)                  │
    │                         │
    ↓                         │ Framework Runtime Libraries
Template Haskell Code Gen     │ (Transaction Builders, Client Lib)
    │                         │ Uses Singletons + Validator Scripts
    ↓                         │ ┌─────────────────┐
┌─────────────────┐           │ │ Transaction     │
│ Validator       │───────────► │ Builders        │
│ Scripts         │ (Input To)  │ (Off-chain)     │
│ (On-chain)      │             └─────────────────┘
└─────────────────┘
```

The magic happens because **everything is derived from the same source of truth** - your type-level specification. This eliminates the possibility of mismatches between different parts of your application.

## Key Architectural Concepts: Singletons and IR

Before diving into the components you'll define (`StateType`, `ValidatorSpec`, etc.), it's helpful to understand two key internal concepts Modsefa uses to bridge the gap between your type-level specification and the final generated code: **Singletons** and the **Intermediate Representation (IR)**.

### Value-Level Singletons (`SAppSpec`, `SActionSpec`, etc.)

Haskell's type system allows us to define application logic at the type level, providing strong compile-time guarantees. However, code generation and runtime libraries (like transaction builders) need to analyze and react to this logic at the *value level*.

**Singletons** are the bridge. They are value-level terms (data structures) that precisely mirror the structure and information of their corresponding type-level specifications. For example, a type-level `ActionSpec` is represented at runtime by an `SActionSpec` value.

**Purpose of Singletons:**

* **Reification:** They bring type-level information down to the value level where it can be processed by standard Haskell functions.
* **Runtime Analysis:** Enable inspection of the application structure (e.g., finding which validator manages a state, extracting action parameters).
* **Input for Compilation:** Serve as the input to the IR compiler.
* **Input for Transaction Building:** Used directly by the transaction builder to understand an action's operations and constraints.

Modsefa provides utilities (`Modsefa.Core.Singletons.Auto`) to automatically generate these singleton values from your type-level specifications, minimizing boilerplate.

### Intermediate Representation (IR - `AppIR`)

After the type-level specification is represented by value-level singletons, a compilation step transforms these singletons into a simpler, backend-agnostic **Intermediate Representation (IR)**, primarily defined by the `AppIR` data type. This compilation is handled by the `Modsefa.Core.IR.Compiler` module.

**Purpose of the IR:**

* **Simplification:** It provides a pure value-level format, abstracting away GADTs and type-level complexities from the singletons, making it easier for code generators to consume.
* **Input for Code Generation:** The IR serves as the direct input for the Template Haskell **validator script generators**. The generators translate the `ValidatorIR`, `ActionIR`, etc., into Plutus Tx code.
* **Analysis & Optimization:** Its simpler structure is suitable for analysis or optimization passes before code generation.

**Crucially, the IR is primarily used for generating the on-chain validator scripts. The off-chain transaction building process relies directly on the value-level singletons (`SAppSpec`, `SActionSpec`) and the generated validator scripts.**

## Core Building Blocks

### 1. State Types: Your Application's Data

**State Types** are the fundamental building blocks of any Modsefa application. They represent data that lives on-chain as UTxOs with specific structure and identification rules.

``` haskell
-- Define your data structures 
data FeedConfig = FeedConfig 
  { feedName :: BuiltinByteString
  , feedOwner :: PubKeyHash 
  }
  
-- Promote them to type-level state specifications 
type FeedConfigState = 'ST "FeedConfig" FeedConfig
```

Each state type must specify:
1. **Data Structure**: What fields and types your state contains
2. **On-chain Representation**: How state instances are represented as UTxOs (containing your data in a datum, locked at a validator address, and identified by specific properties like tokens)
3. **Reference Strategy**: How instances can be found and referenced in transactions

``` haskell
instance StateRepresentable FeedConfigState where
  -- UTxOs identified by holding a specific token
  stateIdentifier _ = TokenIdentified OwnPolicy "FeedConfig" 1
  -- Only one instance allowed - reference as "the unique one"
  type AllowedRefStrategy FeedConfigState = 'OnlyAsUnique
```

### 2. Validators: Managing State Lifecycle

In Cardano, a **validator** is an on-chain script that controls when UTxOs can be spent. In Modsefa, a **Validator Spec** is a higher-level abstraction that declares what state types a validator manages and how it's parameterized.

You don't write the actual validator script logic - instead, you declare the validator's responsibilities, and Modsefa generates the appropriate Cardano validator script automatically.

``` haskell
data FeedValidator

instance ValidatorSpec FeedValidator where
  type Params FeedValidator = '[ '("bootstrapUtxo", TxOutRef) ]
  type ManagedStates FeedValidator = '[FeedConfigState, FeedDataState]
  type ValidatorAppName FeedValidator = "FeedValidator"
  type ValidatorPlutusVersion FeedValidator = 'PlutusV3
  type ValidatorInstanceType FeedValidator = SingleInstance
```

**Key Insight**: The validator's logic is **automatically generated** based on the actions that operate on its managed state. You never have to write validation code manually.

### 3. Actions: What Users Can Do

**Actions** define all the ways users can interact with your application. Each action specifies:
- **Operations**: Create, update, delete, or reference state instances
- **Constraints**: Rules that must be satisfied (signatures, spending conditions, etc.)
- **Parameters**: Data users must provide to execute the action

``` haskell
type InitializeFeedSpec = 
  'ActionSpec @FeedApp "InitializeFeed" 
    '[ 'Op ('Create @FeedConfigState 
         '[ 'SetTo "feedName" ('ParamValue "name")
          , 'SetTo "feedOwner" ('ParamValue "owner") 
          ] 
         '[])
     , 'Op ('Create @FeedDataState 
         '[ 'SetTo "feedData" ('ParamValue "content")
          , 'SetTo "feedStatus" ('EnumValue FeedStatus "Active") 
          ] 
         '[]) 
     ] 
    '[ 'MustSpendValidatorParam "FeedValidator" "bootstrapUtxo" 
     ] 
    '[ '("name", BuiltinByteString)
     , '("owner", PubKeyHash)
     , '("content", BuiltinByteString) 
     ]
```

**What makes this powerful**: The same action specification generates both:
- **Validator logic** that accepts/rejects transactions attempting this action
- **Transaction builder** that constructs valid transactions for this action

### 4. Application Specification: Tying It All Together

The **Application Specification** is where you declare your complete application by listing validators, defining an application-level state machine, and specifying how different parts coordinate.

``` haskell
instance AppSpec FeedApp where
  type Validators FeedApp = '[ 'Validator FeedValidator ]
  type AppStates FeedApp = '["Uninitialized", "Initialized"] 
  type InitialAppState FeedApp = "Uninitialized"
  type ActionTransitions FeedApp =
    '[ '(InitializeFeedSpec, "Uninitialized", "Initialized")
     , '(UpdateFeedSpec, "Initialized", "Initialized")
     ]
  type AppInstanceParameters FeedApp =
    '[ '("FeedValidator", "bootstrapUtxo") ]
  type ParameterDerivations FeedApp = '[]
```

## Multi-Validator Applications: Where Modsefa Shines

Single-validator applications may be simple enough to build manually. Modsefa's real power emerges with **multi-validator applications** where coordination becomes exponentially complex.

### The Coordination Problem

Consider a subscription service with separate validators for:
- **Service configuration** (pricing, terms, owner signatures)
- **User subscriptions** (individual user accounts)
- **Coupon system** (discount codes and redemption)

Traditionally, you'd need to manually:
- Share parameters between validators (service validator address -> coupon validator parameter)
- Coordinate transaction sequences across validators
- Ensure consistent state across the entire application
- Handle complex interdependencies between different parts of the system

### Modsefa's Solution: Automatic Coordination

``` haskell
type ParameterDerivations SubscriptionApp = 
  '[ DeriveParam "CouponValidator" "serviceAddress" 
       (ValidatorAddress "ServiceValidator")
   ]
```

With this single declaration, Modsefa:
- Automatically derives the service validator's address
- Passes it as a parameter to the coupon validator
- Generates transaction builders that handle the coordination
- Ensures all validators receive consistent parameters

### Complex Action Coordination

Actions can span multiple validators seamlessly. For example, an action that creates a new user account while updating service statistics:

``` haskell
type CreateUserAccountSpec = 
  'ActionSpec @SomeApp "CreateUserAccount" 
    '[ 'Op ('Create @UserAccountState [...]) -- User validator 
     , 'Op ('Update @ServiceStatsState [...]) -- Service validator 
     ] 
    '[ 'MustSpendActionParam "donationFee" 
     , 'MustBeSignedByParam "userKey" 
     ] 
    '[...]
```

The framework automatically:
- Determines which validators need to be involved
- Constructs transactions that satisfy all validators
- Handles parameter passing and state consistency
- Generates appropriate redeemers for each validator

## Code Generation: The Magic Behind the Scenes

Modsefa uses **Template Haskell** to analyze your type-level specification and generate code at compile time:

### 1. Validator Logic Generation
For each validator, the framework:
- Analyzes all actions that affect its managed states
- Generates branching logic for different transaction types
- Creates validation rules based on action constraints
- Handles proper redeemer interpretation and state transitions

### 2. Transaction Builder Generation
For each action, the framework:
- Determines required UTxO inputs and outputs
- Calculates appropriate fees and collateral
- Handles multi-validator transaction coordination
- Generates proper redeemers and witnesses

### 3. Client Library Support
The framework also generates:
- Functions for querying and finding state instances on-chain
- Type-safe interfaces for constructing action parameters
- Application instance management utilities

## Type Safety: Eliminating Entire Categories of Bugs

Modsefa's type system provides several layers of safety:

### Compile-Time Validation
- **State references** are checked - you can't reference non-existent states
- **Field access** is validated - typos in field names are caught at compile time
- **Parameter types** are enforced - passing wrong types to actions fails compilation
- **Validator coordination** is verified - parameter derivations must be consistent

### Runtime Guarantees
- **Generated validators** exactly match the specification - no manual coding means no human errors
- **Transaction builders** produce only valid transactions - malformed transactions are impossible
- **Multi-validator consistency** is automatic - coordination bugs are eliminated

## Summary: Why This Approach Works

Modsefa succeeds because it inverts the traditional development model:

**Traditional Approach**: Write validators and transaction builders separately and hope they match
**Modsefa Approach**: Specify behavior once, generate consistent implementations automatically

This eliminates a fundamental source of bugs in Cardano development: the gap between on-chain and off-chain code. When everything is generated from a single specification, consistency is guaranteed by construction.

The type-level specification serves as both:
- **Executable documentation** of your application's behavior
- **Source code** that compiles to working Cardano applications
- **Formal contract** that prevents entire categories of errors

For complex, multi-validator applications, this approach transforms an exponentially complex coordination problem into a manageable specification task.
