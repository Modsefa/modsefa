{-|
Module      : Modsefa.Client.Shortcuts
Description : Convenience functions for creating Modsefa client-side instances.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC

This module provides high-level shortcut functions for common client-side
setup tasks in Modsefa applications:

- 'createAppInstance'': Creates an 'SAppInstance' by automatically deriving the 'SAppSpec' using 'Modsefa.Core.Singletons.Auto.autoSingletonFull'.

These functions reduce boilerplate by requiring only the application type and
the necessary instance parameters, assuming the application type satisfies
the required 'Modsefa.Core.Singletons.Auto.AutoSingleton*' constraints.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Modsefa.Client.Shortcuts
  ( createAppInstance'
  ) where
  
import Data.Typeable (Typeable)
import GHC.TypeLits (KnownSymbol)

import Modsefa.Core.Foundation(AppSpec(..), ParamsToValue, ResolveInstanceParamList)
import Modsefa.Core.Singletons
  ( AutoSingletonActionTransitionList, AutoSingletonAppInstanceParams
  , AutoSingletonAppStateList, AutoSingletonParamDerivationList
  , AutoSingletonValidators, SAppInstance, autoSingletonFull, createAppInstance
  )


-- | Creates an 'SAppInstance' for a given application type @app@ using automatic singleton derivation.
-- This function simplifies instance creation by automatically generating the 'SAppSpec'
-- using 'autoSingletonFull', requiring only the application type @app@ and the
-- resolved instance parameters.
--
-- Requires that the application type @app@ satisfies all necessary 'AutoSingleton*' constraints
-- for its components ('Validators', 'AppStates', 'ActionTransitions', etc.).
createAppInstance' :: forall app.
                    ( AppSpec app
                    , KnownSymbol (InitialAppState app)
                    , AutoSingletonValidators (Validators app)
                    , AutoSingletonAppStateList (AppStates app)
                    , AutoSingletonActionTransitionList (ActionTransitions app)
                    , AutoSingletonAppInstanceParams (AppInstanceParameters app)
                    , AutoSingletonParamDerivationList (ParameterDerivations app)
                    , Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
                    , Show (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
                    )
                 => ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)) -- ^ The resolved value-level instance parameters.
                 -> SAppInstance app -- ^ The resulting application instance singleton.
createAppInstance' instanceParams' =
  -- Automatically derive the full SAppSpec singleton for the application type.
  let appSpec' = autoSingletonFull @app
  -- Create the SAppInstance using the derived spec and provided parameters.
  in createAppInstance appSpec' instanceParams'