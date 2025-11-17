{-|
Module      : Modsefa.CodeGen.Generation
Description : Re-export module for Modsefa code generation.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module serves as the primary entry point for the Modsefa code generation subsystem.
It re-exports the main functionalities related to generating Plutus Tx validator scripts
from the Modsefa Intermediate Representation (IR) or directly from specifications.
This includes modules for:

- Overall validator generation orchestration ('Modsefa.CodeGen.Generation.Validators')
- Generating the core on-chain logic ('Modsefa.CodeGen.Generation.Logic')
- Generating on-chain constraint validation code ('Modsefa.CodeGen.Generation.Constraints')

Import this module for convenient access to code generation features, particularly Template Haskell functions:

@
import Modsefa.CodeGen.Generation
@
-}
-- | Code generation for the Modsefa library
--
-- This module provides the complete code generation API for the Modsefa
-- library. It re-exports functionality from specialized Generation modules
-- to provide a convenient single import point for Template Haskell code
-- generation operations.
--
-- The Generation layer is designed to:
--   - Provide a stable API for Template Haskell code generation
--   - Consolidate commonly used code generation imports
--   - Separate different aspects of code generation into focused modules
--   - Enable incremental compilation of code generation logic
--
-- Usage:
--   Import this module to get access to all code generation functionality:
--
--   @
--   import Modsefa.CodeGen.Generation
--   @
--
--   This gives you access to all functions needed for most code generation
--   operations, particularly validator generation.
module Modsefa.CodeGen.Generation
  ( -- * Re-exports from Generation modules
    module Modsefa.CodeGen.Generation.Constraints
  , module Modsefa.CodeGen.Generation.Logic
  , module Modsefa.CodeGen.Generation.State
  , module Modsefa.CodeGen.Generation.Validators
  ) where

-- Generation modules re-exported
import Modsefa.CodeGen.Generation.Constraints
import Modsefa.CodeGen.Generation.Logic
import Modsefa.CodeGen.Generation.State
import Modsefa.CodeGen.Generation.Validators