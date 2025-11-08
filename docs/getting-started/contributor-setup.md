---
title: "Contributor Setup"
description: "Setting up your local environment to build and contribute to Modsefa."
order: 1
---

Modsefa is built on a foundation of modern Haskell and Nix tooling. The recommended way to develop with Modsefa is by using a Nix-based development shell, which ensures you have all the correct dependencies and GHC versions.

This guide is for **contributors** or anyone who wishes to build the Modsefa project from the source. If you want to use Modsefa as a library in your own application, please see the **[Quick Start](./quick-start.md)** guide.

### **1. Requirements**

-   **Nix:** You must have the [Nix package manager](https://nixos.org/download.html) installed with `flakes` enabled.
-   **Git:** To clone the repository.

### **2. Initial Setup**

1.  **Clone the Repository:**
    ```bash
    git clone https://github.com/Modsefa/modsefa
    cd modsefa
    ```

2.  **Enter the Development Shell:**
    This command will build all the dependencies (including the correct GHC version, Cabal, and HLS) and drop you into a shell where all tools are available.

    ```bash
    nix develop
    ```

### **3. Environment Setup (For Running Examples)**

Before you can run the example transactions, you need to set up your local environment.

1.  **Connect to a Node & Services:**
    You must have access to a Cardano environment. The client is configured to connect to services like **Ogmios** and **Kupo**. This can be against a public testnet or a local devnet (such as one run by [Yaci Devkit](https://github.com/bloxbean/yaci-devkit)).

2.  **Update Configuration:**
    The `config.json` file in the root of the repository tells the client how to connect to these services. You will need to edit this file to:
    * Point the `ogmiosUrl` and `kupoUrl` to your running services.
    * Ensure the `networkId` (e.g., "testnet") matches your environment.

3.  **Generate Keys:**
    The example tests require a signing key named `spender`. The `nix develop` shell provides a `keygen` command for this (which is an alias defined in `nix/shell-hook.sh`).

    ```bash
    keygen spender
    ```
    This will create the necessary `keys/spender.skey` and `keys/spender.vkey` files.

### **4. Build and Interact**

1.  **Build the Project:**
    Once inside the shell, you can build all components using Cabal:

    ```bash
    cabal build all
    ```

2.  **Run the Example Transactions (via GHCI):**
    The test suites are designed to be run interactively from `ghci`.

    First, start a REPL for the `modsefa-examples` package:
    ```bash
    cabal repl modsefa-examples
    ```

    Once `ghci` has loaded, you can load the test module and run the example functions. **Note:** You will need a "bootstrap" UTxO (a UTxO in your `spender` wallet) to initialize the app instance.

    ```haskell
    -- Load the Feed app test module
    ghci> +m Modsefa.Examples.Feed.Test

    -- Run the initialization action
    -- (Replace the Tx hash and index with your own bootstrap UTxO)
    ghci> runFeedInitTest "000011112222..." 0

    -- After it succeeds, you can run an update action
    ghci> runFeedUpdateTest "000011112222..." 0
    ```