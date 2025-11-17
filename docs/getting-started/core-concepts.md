---
title: "Core Concepts"
description: "Understanding the fundamental building blocks of a Modsefa application."
order: 2
---

# Core Concepts

Welcome to Modsefa! Before you dive into writing your first dApp, it's helpful to understand the core concepts that underpin the framework. Modsefa is built on a **"spec-first"** philosophy: you declare your entire application's logic at the type level, and Modsefa generates the on-chain and off-chain code for you.

This approach provides incredible safety and clarity. Let's meet the five key building blocks you'll use to create your specification.

---

## 1. `AppSpec` - The Blueprint

The **`AppSpec`** is the highest-level container for your entire application. Think of it as the master blueprint for a house. It doesn't describe the details of every single room, but it defines how many rooms there are, how they connect, and the overall structure.

Specifically, the `AppSpec` declares:
-   All the validators that belong to the application
-   The application's high-level state machine (e.g., `Uninitialized` -> `Active` -> `Paused`)
-   How user actions trigger transitions between these states

---

## 2. `ValidatorSpec` - The Smart Contract

A **`ValidatorSpec`** is the specification for a single on-chain validator (a smart contract). If the `AppSpec` is the blueprint for the house, a `ValidatorSpec` is the detailed plan for a specific room, like the kitchen.

It defines:
-   **Parameters:** What configuration is needed to build and deploy this validator (e.g., an owner's public key)
-   **Managed States:** Which types of on-chain data this validator is responsible for. The complete on-chain logic that governs these states is derived from the collection of all ActionSpecs that interact with them

---

## 3. `StateSpec` - The On-Chain Data

A **`StateSpec`** (State Specification) is the specification for a single piece of on-chain data (a datum). It defines the properties of the data, from which Modsefa generates the actual Haskell datum type for you.

It specifies:
-   The datum's fields and their types (e.g., an `author` field of type `PubKeyHash` and a `content` field of type `BuiltinByteString`).
-   How instances are identified on-chain (e.g., by a UTxO containing a specific NFT).
-   How the state can be referenced (e.g., as a unique singleton, or by its properties).

---

## 4. `ActionSpec` - The User Interaction

An **`ActionSpec`** is where the magic happens. It defines a single, specific action a user can take in your dApp. This is the verb of your application. In our house, an `ActionSpec` would be "opening the refrigerator" or "turning on the stove."

An `ActionSpec` brings everything together by declaring:
-   **Operations:** The specific changes to make to on-chain states (Create, Update, Delete)
-   **Constraints:** The rules that must be met for the action to be valid (e.g., "must be signed by the owner")
-   **Parameters:** The data the user must provide to perform the action (e.g., the content of a new blog post)

Modsefa analyzes the entire collection of `ActionSpec`s associated with a validator. From this complete set of possible interactions, it generates a single, unified on-chain validator script that correctly handles all specified actions, along with the corresponding off-chain client code for building transactions.

---

## How It All Fits Together

You, the developer, define these four specifications at the type level in Haskell.

1.  You start by defining your **`StateSpec`s** (the data specifications)
2.  You group them under **`ValidatorSpec`s** (the contracts that manage the data)
3.  You define **`ActionSpec`s** (the user interactions that change the data)
4.  Finally, you bring all your validators and actions together in the main **`AppSpec`** (the complete dApp)

Once your spec is complete, Modsefa's code generator takes over, creating everything you need to compile, deploy, and interact with your dApp, all while ensuring that the code perfectly matches the rules you laid out in your specification.

Ready to see it in action? Let's move on to the **Quick Start** guide!