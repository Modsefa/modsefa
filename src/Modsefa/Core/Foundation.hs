{-|
Module      : Modsefa.Core.Foundation
Description : Foundation layer re-export module for Modsefa.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module serves as the main entry point for the Modsefa foundation layer.
It re-exports all core types ('Modsefa.Core.Foundation.Types'),
type families ('Modsefa.Core.Foundation.TypeFamilies'), and constraint
aliases ('Modsefa.Core.Foundation.Constraints') needed for defining
and working with Modsefa smart contract specifications.

Import this module for convenient access to all foundational elements:

@
import Modsefa.Core.Foundation
@
-}
module Modsefa.Core.Foundation
  ( -- * Re-exports
    module Modsefa.Core.Foundation.App
  , module Modsefa.Core.Foundation.Constraints
  , module Modsefa.Core.Foundation.TypeFamilies
  , module Modsefa.Core.Foundation.Types
  , module Modsefa.Core.Foundation.Validator
  ) where

-- Foundation modules
import Modsefa.Core.Foundation.App
import Modsefa.Core.Foundation.Constraints
import Modsefa.Core.Foundation.TypeFamilies  
import Modsefa.Core.Foundation.Types
import Modsefa.Core.Foundation.Validator