---
title: "Quick Start"
description: "Using Modsefa as a library in your new project."
order: 3
---

# Modsefa Quick Start Guide

This guide will walk you through setting up a new Haskell project that uses the Modsefa framework for building Cardano applications.

## Prerequisites

* **Nix package manager** with flakes enabled ([installation guide](https://nixos.org/download.html))
* **Git**

All other tools (GHC, Cabal, HLS) will be provided through the Nix development environment.

## Step 1: Create Project from Template

Create a new directory and initialize it with the haskell.nix template:

```bash
mkdir my-cardano-project
cd my-cardano-project
nix flake init --template templates#haskell-nix --impure
```

**This creates the following files:**

- `hello.cabal` - Package definition
- `flake.nix` - Nix flake configuration
- `nix/hix.nix` - Haskell-specific Nix configuration
- `src/hello.hs` - Source code
- `LICENSE` - License file
- `Setup.hs` - Cabal setup script

**Note on naming:** The template creates a project named "hello". You can rename it by changing the following:

- Rename `hello.cabal` to `your-project-name.cabal`
- Change the `name:` field inside the `.cabal` file
- Update the package reference in `flake.nix` (line with `default = flake.packages."hello:exe:hello"`)
- Change the `packages:` field inside the `cabal.project` file that we will create below to match your `cabal` file name

## Step 2: Update `nix/hix.nix`

We recommend making the following changes to the generated `nix/hix.nix` file:

1. Change `compiler-nix-name = "ghc926"` to `"ghc966"`
2. Uncomment `shell.tools.haskell-language-server = "latest"` and `shell.tools.hlint = "latest"`
3. Add `shell.buildInputs` section with the system libraries needed by Cardano packages

```nix
{pkgs, ...}: {
  compiler-nix-name = "ghc966";

  # Tools to include in the development shell
  shell.tools.cabal = "latest";
  shell.tools.hlint = "latest";
  shell.tools.haskell-language-server = "latest";

  shell.buildInputs = with pkgs; [
    nix-prefetch-git
    libsodium
    secp256k1
    blst
    zlib
  ];
}
```

## Step 3: Update `flake.nix`

The template's `flake.nix` needs to be modified to include CHaP (Cardano Haskell Packages) and handle some additional system dependencies. Here are the key changes you'll need to make:

1. Add CHaP as an input (in the `inputs` section)
2. Add CHaP to the `inputMap` (so cabal can find Cardano packages)
3. Add an overlay for `libblst` package config mapping
4. Change the default package from `"hello:exe:hello"` to `"hello:lib:hello"` (since we'll be creating a library, not an executable)

```nix
{
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.CHaP = {
    url = "github:intersectMBO/cardano-haskell-packages?ref=repo";
    flake = false;
  };
  
  outputs = { self, nixpkgs, flake-utils, haskellNix, CHaP }:
    let
      supportedSystems = [
        "x86_64-linux"
      ];
    in
      flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlay = final: prev: {
          haskell-nix = prev.haskell-nix // {
            extraPkgconfigMappings = prev.haskell-nix.extraPkgconfigMappings // {
                "libblst" = [ "blst" ];
            };
          };
        };

        overlays = [ 
          haskellNix.overlay
          (final: prev: {
            hixProject = final.haskell-nix.hix.project {
              src = ./.;
              evalSystem = "x86_64-linux";
              inputMap = { 
                "https://chap.intersectmbo.org/" = CHaP; 
              };
            };
          })
          overlay
        ];
        
        pkgs = import nixpkgs { 
          inherit system overlays; 
          inherit (haskellNix) config; 
        };
        
        flake = pkgs.hixProject.flake {};
      in 
      flake // {
        legacyPackages = pkgs;
        packages = flake.packages // { default = flake.packages."hello:lib:hello"; };
      });

  nixConfig = {
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}
```

## Step 4: Update `hello.cabal`

The template creates an executable package. We'll convert it to a library package since that's the typical structure for projects using Modsefa. Here are the key changes you'll need to make:

1. Update `cabal-version` to `3.6` (for modern Cabal features)
2. Replace the `executable` section with a `library` section
3. Set correct base version bounds: `base >= 4.14 && < 5`
4. Add `atlas-cardano` as a dependency (required by Modsefa)
5. Add Modsefa dependencies: `modsefa:modsefa-lib` and `modsefa:modsefa-client`
6. Change `main-is: hello.hs` to `exposed-modules: MyLib`
7. Update the `license` field to use a valid SPDX license identifier (the template uses `BSD3` which is deprecated; `BSD-3-Clause` is the modern equivalent, or choose any license appropriate for your project)

Your `hello.cabal` should look similar to this:

```cabal
cabal-version: 3.6
name: hello
version: 1.0.0.2
license: Apache-2.0
license-file: LICENSE
author: Simon Marlow
maintainer: Simon Marlow <marlowsd@gmail.com>
synopsis: Hello World, an example package
category: Console, Text
build-type: Simple

library
  hs-source-dirs: src
  exposed-modules: MyLib
  build-depends:
      base >= 4.14 && < 5
    , atlas-cardano
    , modsefa:modsefa-lib
    , modsefa:modsefa-client
  default-language: Haskell2010
```

## Step 5: Create `cabal.project`

The template doesn't include a `cabal.project` file, but we need one to configure CHaP and specify all the Cardano dependencies.

Create a file named `cabal.project` in your project root with the following content. This configuration specifies:

- The CHaP repository and its security keys
- Index states that pin package versions for reproducibility
- Source repository packages for specific versions of dependencies (Atlas, Maestro SDK, CLB, and Modsefa)
- Package flags and build settings

Note: The example below shows the complete file that your project needs. You may have to change the `packages` field to match your project's cabal file.

```cabal
repository cardano-haskell-packages
  url: https://chap.intersectmbo.org/
  secure: True
  root-keys:
    3e0cce471cf09815f930210f7827266fd09045445d65923e6d0238a6cd15126f
    443abb7fb497a134c343faf52f0b659bd7999bc06b7f63fa76dc99d631f9bea1
    a86a1f6ce86c449c46666bda44268677abf29b5b2d2eb5ec7af903ec2f117a82
    bcec67e8e99cabfa7764d75ad9b158d72bfacf70ca1d0ec8bc6b4406d1bf8413
    c00aae8461a256275598500ea0e187588c35a5d5d7454fb57eac18d9edb86a56
    d4a35cd3121aa00d18544bb0ac01c3e1691d618f462c46129271bccf39f7e8ee

packages: ./hello.cabal

index-state: 2025-04-15T19:49:23Z

index-state:
  , hackage.haskell.org 2025-04-15T19:49:23Z
  , cardano-haskell-packages 2025-04-11T16:42:25Z

package cardano-crypto-praos
  flags: -external-libsodium-vrf

-- Modsefa framework
source-repository-package
  type: git
  location: https://github.com/Modsefa/modsefa
  tag: v0.1.0
  --sha256: sha256-pRF214loCkJNzdj/PfSCqrSyttePKPW2pK6shJERSL4=

source-repository-package
  type: git
  location: https://github.com/geniusyield/atlas
  tag: v0.13.0
  --sha256: sha256-PAogmaPNQC22n5PjwLaRXDv2Xo+TasFJCJn0RJruvCA=

source-repository-package
  type: git
  location: https://github.com/maestro-org/haskell-sdk
  tag: 3e39a6d485d7c6f98222b1ca58aed2fb45e5ff27
  --sha256: sha256-plfrSgirKf7WGESYvEBqBkR1s673Qd0ZhGs0KzGfOig=

source-repository-package
  type: git
  location: https://github.com/mlabs-haskell/clb
  tag: f33e486279029c6dc38a4527ff587443f8cdf373
  --sha256: sha256-77CoSzJEwZDGtkcquPcP6INdO2eDHqAwZ8o04ybMhrQ=
  subdir: clb

source-repository-package
  type: git
  location: https://github.com/IntersectMBO/cardano-addresses
  tag: d611632fc3d616d5b4038a70f864bf2613c125d0
  --sha256: sha256-vQ2XB95kw05IZuRkyK4cPQtaKZ1bZAoLtN9GrOOwQvM=

allow-newer:
    katip:Win32
  , ekg-wai:time

package cryptonite
  flags: -support_rdrand

package cardano-node
  flags: -systemd

package bitvec
  flags: -simd
```

### Using a Different Modsefa Version

If you want to use a different version of Modsefa (or any other `source-repository-package`), you'll need to update the `tag` and recalculate the `sha256` hash.

**To get the hash for a different version:**

```bash
# Replace v0.1.0 with your desired version
nix-prefetch-git https://github.com/Modsefa/modsefa --rev v0.1.0

# The output will show you the sha256 hash
# Convert it to SRI format:
nix hash to-sri --type sha256 <hash-from-previous-command>
```

Then update your cabal.project:
- Change the `tag` field to your desired version
- Update the `--sha256` line with the hash from above

## Step 6: Update Source Code

Rename `src/hello.hs` to `src/MyLib.hs` and replace its contents with a minimal example that imports Modsefa:

```bash
mv src/hello.hs src/MyLib.hs
```

**Edit `src/MyLib.hs`:**

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module MyLib (main) where

import Data.Proxy (Proxy(Proxy))
import GHC.Generics (Generic)
import GHC.TypeLits (symbolVal)

import Modsefa.Core.Foundation (GetStateName, StateType(ST))

-- Define a state type and extract its name using Modsefa's type-level functions
type Test = GetStateName ('ST "TestState" Integer)

main :: IO ()
main = putStrLn (symbolVal @Test Proxy)
```

## Step 7: Enter Development Shell and Build

Now you're ready to build your project:

```bash
# Enter the Nix development shell (first time will take a while)
nix develop

# Build the project
cabal build
```

## Next Steps

Congratulations! You now have a working Haskell project with Modsefa configured. Here's what to explore next:

1. Study the Examples: Check out the complete examples in the [Modsefa repository on GitHub](https://github.com/Modsefa/modsefa):

    - Feed App: `examples/Modsefa/Examples/Feed/`
    - Subscription App: `examples/Modsefa/Examples/Subscription/`
2. Read the Tutorials: Work through the detailed tutorials in the Modsefa documentation
3. Explore the API: Browse the Modsefa API documentation to understand available modules and functions
4. Build Your Application: Start defining your own Cardano smart contract using Modsefa's specification-driven approach