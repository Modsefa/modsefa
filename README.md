# Modsefa

Modsefa is a **specification-first Haskell framework** for building secure, scalable, and verifiable decentralized applications (dApps) on the Cardano blockchain. It leverages Haskell's advanced type system to let you define your entire application—states, validators, actions, and constraints—at the **type level**.

This "correct-by-construction" approach means that if your application compiles, your on-chain and off-chain code are **guaranteed to be consistent**, eliminating an entire class of common and costly runtime errors.

## Key Features
- **Correct-by-Construction**: Define your app's logic as a single type-level specification. Because both the validator scripts and the transaction builders are derived from this single source, runtime mismatches between on-chain rules and off-chain construction are prevented by construction. Specification errors are caught at compile time.

- **Automated Code Generation:** Modsefa automatically generates validator scripts and provides type-safe transaction builders directly from your unified specification.

- **Simplified dApp Logic:** Focus on your business logic, not Plutus boilerplate. Express complex, multi-validator actions in a single, declarative action specification.

- **Integrated Testing Utilities:** Comes with a powerful testing library designed to corrupt valid transactions, allowing you to systematically prove your validators are robust against edge cases and malicious attacks.

- **Seamless Multi-Validator Coordination:** Define actions that atomically span multiple validators and states. The framework handles the complex transaction building, parameter passing, and instance consistency checks for you.

## Getting Started

### Prerequisites

This project is built using Nix flakes.

1. **Install Nix:** Follow the instructions at [nix.dev](https://nix.dev/).
2. **Enable Flakes:** Enable the `nix-command` and `flakes` experimental features.

### Development

You can enter a development shell with all dependencies available by running:

```bash
nix develop
```

From within the shell, you can use `cabal` to build the project:

```bash
cabal build all
```

## Documentation

All documentation, including tutorials, API references, and the design philosophy, is located in the [/docs](./docs/) directory.

We strongly recommend starting with the tutorials to understand the Modsefa development model:

1. Build a simple, single-validator Data Feed application. This covers the fundamentals of defining states, actions, and generating code.

2. Build a more advanced multi-validator Subscription application. This explores concepts like state aggregation, parameter derivation, and complex action coordination.

Working code for both tutorials can be found in the directory.

## Contributing

Contributions are welcome! Please feel free to open an issue or submit a pull request. (A formal `CONTRIBUTING.md` guide will be added in the future).

## License

Modsefa is licensed under the [Apache 2.0 License](./LICENSE).