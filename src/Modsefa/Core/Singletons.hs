{-|
Module      : Modsefa.Core.Singletons
Description : Re-export module for the Modsefa singleton system.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module serves as the primary entry point for the Modsefa singleton system.
It re-exports all core singleton types ('Modsefa.Core.Singletons.Types'),
analysis functions ('Modsefa.Core.Singletons.Analysis'), and automatic
derivation mechanisms ('Modsefa.Core.Singletons.Auto').

Import this module for convenient access to all singleton-related functionality:

@
import Modsefa.Core.Singletons
@
-}
module Modsefa.Core.Singletons
  ( module Modsefa.Core.Singletons.Analysis
  , module Modsefa.Core.Singletons.Auto    
  , module Modsefa.Core.Singletons.Types
  ) where

import Modsefa.Core.Singletons.Analysis
import Modsefa.Core.Singletons.Auto
import Modsefa.Core.Singletons.Types