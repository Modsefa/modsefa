{-|
Module      : Modsefa.CodeGen.Generation.State
Description : Template Haskell generators for state data types and instances.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC (requires TemplateHaskell)

This module provides the top-level Template Haskell splice
'generateAllDatumsForApp', which generates all necessary Haskell data type
declarations, Plutus instances, and 'StateDatum' type family instances
for an entire Modsefa application from its type-level specification.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Modsefa.CodeGen.Generation.State
  ( -- * Datum Generation Splices
    generateStateDatum
  , generateAllDatumsForApp
    -- * Instance Generation Splice
  , generateStateInstances
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Proxy (Proxy(Proxy))
import Data.Text (unpack)
import Data.Typeable (Typeable, typeRep)
import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)
import Language.Haskell.TH
  ( Bang(Bang), Con(NormalC, RecC), Dec(DataD, TySynInstD), DerivClause(DerivClause)
  , Info (TyConI), Name, Q, SourceStrictness(NoSourceStrictness)
  , SourceUnpackedness(NoSourceUnpackedness), TySynEqn(TySynEqn), Type(AppT, ConT)
  , VarBangType, conT, mkName, nameBase, reify
  )

import PlutusTx (makeIsDataIndexed, makeLift)

import Modsefa.Core.Foundation
  ( AllManagedStates, AppSpec, StateDatum, StateSpec, Validators
  )
import Modsefa.Core.IR.Compiler (compileStateList)
import Modsefa.Core.IR.Types(DatumFieldIR(..), StateInfoIR(..))
import Modsefa.Core.Singletons 
  ( AutoSingletonStateSpec, SStateList(..), autoSingletonStateSpec
  )
import Modsefa.Core.Transaction (Mappable)


-- ============================================================================
-- * Top-Level App Generator
-- ============================================================================

-- | Generates all datum 'data' declarations for an entire 'AppSpec'.
-- | This MUST be followed by a separate call to '$(generateStateInstances)'
-- | in the same module to generate the required instances.
-- |
-- | Usage:
-- |   $(generateAllDatumsForApp @FeedApp)
-- |   $(generateStateInstances)
-- |
generateAllDatumsForApp :: forall app.
  ( AppSpec app
  , GenerateDatumTypes (AllManagedStates (Validators app))
  ) => Q [Dec]
generateAllDatumsForApp = do
    generateDatums' @(AllManagedStates (Validators app))

  -- | Helper class to generate datum types from a list of states
class GenerateDatumTypes (states :: [Data.Kind.Type]) where
  generateDatums' :: Q [Dec]

instance GenerateDatumTypes ('[] :: [Data.Kind.Type]) where
  generateDatums' = pure []

instance (StateSpec s, AutoSingletonStateSpec s, Typeable s, GenerateDatumTypes rest)
      => GenerateDatumTypes ((s ': rest) :: [Data.Kind.Type]) where
  generateDatums' = do
    headDecs <- generateStateDatum @s
    restDecs <- generateDatums' @rest
    return (headDecs ++ restDecs)

-- ============================================================================
-- * Per-State Generators
-- ============================================================================

-- | A global registry to hold (DatumName, StateTagName, IsMappable) tuples.
-- This allows 'generateStateDatum' to register types that 'generateStateInstances'
-- can later process in a separate splice.
{-# NOINLINE stateRegistry #-}
stateRegistry :: IORef [(Name, Name, Bool)]
stateRegistry = unsafePerformIO (newIORef [])

-- | Top-level splice to generate the 'data' declaration for a
-- | single state from its 'StateSpec' instance.
-- |
-- | This registers the type with the 'stateRegistry' and
-- | MUST be used in conjunction with a later call to 'generateStateInstances'.
-- |
-- | Usage:
-- |
-- | @
-- | data MyState
-- | instance StateSpec MyState where ...
-- | $(generateStateDatum @MyState)
-- | @
generateStateDatum :: forall s. (StateSpec s, AutoSingletonStateSpec s, Typeable s) => Q [Dec]
generateStateDatum = do
  -- 1. Create a singleton for just this state
  let stateSingleton = autoSingletonStateSpec @s
  
  -- 2. Compile *just* this state to IR
  -- We wrap it in a list to reuse the existing compileStateList function
  let irList = compileStateList (SSCons stateSingleton SSNil)
  
  -- 3. Generate code from that single IR
  case irList of
    [info] -> do
      let datumName = mkName (unpack (stateInfoDatumName info))
      let stateTypeName = mkName (unpack (stateInfoName info))
      let isMappable = stateInfoMappable info

      liftIO $ modifyIORef' stateRegistry ((datumName, stateTypeName, isMappable) :)
      
      -- This call remains the same, generating only the data type
      generateSingleDatum info
    _      -> fail $ "Internal error: Compiling state " ++ show (typeRep (Proxy @s)) ++ " failed."

-- | Top-level splice to generate all instances (makeLift, makeIsDataIndexed,
-- | StateDatum, Mappable) for all 'data' types previously registered
-- | by calls to 'generateStateDatum'.
-- |
-- | Usage:
-- |
-- | @
-- | $(generateStateDatum @StateA)
-- | $(generateStateDatum @StateB)
-- | $(generateStateInstances) -- Generates instances for types created by StateA and StateB
-- | @
generateStateInstances :: Q [Dec]
generateStateInstances = do
  -- 1. Read and clear the registry
  registeredPairs <- liftIO $ readIORef stateRegistry
  liftIO $ writeIORef stateRegistry []
  
  -- 2. Generate instances for each pair
  concat <$> mapM generateInstancesForPair registeredPairs

-- ============================================================================
-- * Internal Helpers
-- ============================================================================

-- | (Internal) Generates all instance declarations for a single
-- | (DatumName, StateTagName, IsMappable) tuple from the registry.
generateInstancesForPair :: (Name, Name, Bool) -> Q [Dec]
generateInstancesForPair (datumName, stateTypeName, isMappable) = do
  -- 1. Reify the data type to get its *actual* constructor Name
  info <- reify datumName
  conName <- case info of
    TyConI (DataD _ _ _ _ [RecC conName _] _) -> return conName
    TyConI (DataD _ _ _ _ [NormalC conName _] _) -> return conName
    TyConI (DataD _ _ _ _ [con] _) -> 
      fail $ "Expected RecC or NormalC constructor for " ++ nameBase datumName ++ ", but got " ++ show con
    TyConI (DataD _ _ _ _ cons _) -> 
      fail $ "Expected exactly one constructor for " ++ nameBase datumName ++ ", but got " ++ show (length cons)
    _ -> 
      fail $ "Could not reify constructor for type: " ++ nameBase datumName

  -- 2. Generate Plutus instances (makeLift, makeIsDataIndexed)
  liftDecls <- PlutusTx.makeLift datumName
  isDataDecls <- PlutusTx.makeIsDataIndexed datumName [(conName, 0)]
    
  -- 3. Generate StateDatum Type Family Instance
  let stateDatumInst = TySynInstD
                         (TySynEqn Nothing
                           (AppT (ConT ''StateDatum) (ConT stateTypeName))
                           (ConT datumName))
                           
  -- 4. Generate Mappable Instance (if requested)
  mappableInsts <- if isMappable
    then [d| instance Mappable $(conT datumName) |]
    else return [] -- No instance

  -- 5. Combine all declarations
  return (liftDecls ++ isDataDecls ++ [stateDatumInst] ++ mappableInsts)

-- | (Internal) Generates the 'data' type declaration for a single state from its IR.
-- (Instances are handled separately by 'generateStateInstances').
generateSingleDatum :: StateInfoIR -> Q [Dec]
generateSingleDatum info = do
  -- Create TH Names for the state and its datum
  let datumNameStr = unpack (stateInfoDatumName info)
  let datumName = mkName datumNameStr

  -- Generate Data Declaration
  -- data MyDatum = MyDatum { field :: Type } deriving (Eq, Show, Generic)
  let fields = map compileField (stateInfoFields info)
  let con = RecC datumName fields
  let deriv = [DerivClause Nothing [ConT ''Eq, ConT ''Show, ConT ''Generic]]
  let dataDecl = DataD [] datumName [] Nothing [con] deriv

  -- Combine all declarations for this state
  return [dataDecl]

-- | (Internal) Helper to compile a single field definition from IR to TH 'VarBangType'.
-- (A VarBangType is the (FieldName, Bang, Type) tuple used in records)
compileField :: DatumFieldIR -> VarBangType
compileField (DatumFieldIR name typeStr) =
  ( mkName (unpack name)
  , Bang NoSourceUnpackedness NoSourceStrictness
  , ConT (mkName (unpack typeStr)) -- NOTE: Assumes simple type names
  )