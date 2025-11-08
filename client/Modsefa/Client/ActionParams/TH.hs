{-|
Module      : Modsefa.Client.ActionParams.TH
Description : Template Haskell functions for generating ToSParamTuple instances.
Copyright   : (c) The Modsefa Project, 2025
License     : Apache-2.0
Maintainer  : nrmeyer@ptrx.xyz
Stability   : experimental
Portability : GHC (requires TemplateHaskell)

This module provides the Template Haskell function 'makeToSParamTupleInstances'
used to automatically generate boilerplate instances for the 'ToSParamTuple'
typeclass from "Modsefa.Client.ActionParams".
-}
module Modsefa.Client.ActionParams.TH
  ( makeToSParamTupleInstances
  ) where

import Data.Typeable (Typeable)
import Language.Haskell.TH
  ( Dec, Exp, Name, Q, Quote(newName), Type, appT, clause, cxt, funD, instanceD
  , normalB, promotedConsT, promotedNilT, tupP, tupleT, varE, varP
  , varT
  )

import PlutusLedgerApi.V3 (ToData)

import Modsefa.Core.Singletons (SParamTuple(..))

import Modsefa.Client.ActionParams.Types (ToSParamTuple(..))


-- | Generates 'ToSParamTuple' instances for tuples of arity 2 up to 'n'.
--   e.g., $(makeToSParamTupleInstances 15) generates instances for 2-tuples up to 15-tuples.
makeToSParamTupleInstances :: Int -> Q [Dec]
makeToSParamTupleInstances n = mapM genInstance [2..n]

-- | Generates a single 'ToSParamTuple' instance for a tuple of arity 'k'.
genInstance :: Int -> Q Dec
genInstance k = do
  -- Generate 'k' type variable names for types (e.g., t1, t2, ...)
  tVars <- mapM ((newName . ("t" ++)) . show) [1..k]
  -- Generate 'k' type variable names for symbols (e.g., n1, n2, ...)
  nVars <- mapM ((newName . ("n" ++)) . show) [1..k]

  -- 'context' is already Q Cxt
  let context = cxt (concatMap buildContext tVars)

  -- 'instanceHead' is already Q Type
  let instanceHead = buildInstanceHead tVars nVars

  -- Generate 'k' value variable names (e.g., v1, v2, ...)
  vVars <- mapM ((newName . ("v" ++)) . show) [1..k]

  -- 'funImpl' is 'Q Dec'
  let funImpl = buildFunImpl vVars

  -- Create the instance declaration
  -- 'instanceD' expects: Q Cxt -> Q Type -> [Q Dec]
  -- GHC will automatically quantify over the free type variables (tVars, nVars)
  -- when it splices this declaration.
  instanceD context instanceHead [funImpl]

-- | Builds the context for a single type variable: (Typeable t, ToData t, Eq t)
buildContext :: Name -> [Q Type]
buildContext t =
  [ [t| Typeable $(varT t) |]
  , [t| ToData $(varT t) |]
  , [t| Eq $(varT t) |]
  ]

-- | Builds the instance head: ToSParamTuple (t1, t2, ...) '[ '(n1, t1), '(n2, t2), ...]
buildInstanceHead :: [Name] -> [Name] -> Q Type
buildInstanceHead tVars nVars = do
  let k = length tVars

  -- 1. Build the tuple type: (t1, t2, ...)
  let tupleType = applyT (tupleT k) (map varT tVars)

  -- 2. Build the promoted list type: '[ '(n1, t1), '(n2, t2), ...]
  let paramTypes = zipWith buildParamTuple (map varT nVars) tVars
  let paramListType = buildPromotedList paramTypes

  -- 3. Build the final head: ToSParamTuple ... ...
  [t| ToSParamTuple $tupleType $paramListType |]

-- | Helper to build a promoted type-level tuple: '(n, t)
buildParamTuple :: Q Type -> Name -> Q Type
buildParamTuple nameT t = [t| '($nameT, $(varT t)) |]

-- | Helper to build a promoted type-level list
buildPromotedList :: [Q Type] -> Q Type
buildPromotedList = foldr (\t acc -> [t| $promotedConsT $t $acc |]) promotedNilT

-- | Helper to apply a type constructor to a list of types
applyT :: Q Type -> [Q Type] -> Q Type
applyT = foldl appT

-- | Builds the implementation: toSParamTuple (v1, v2, ...) = STupleCons v1 (STupleCons v2 (...))
buildFunImpl :: [Name] -> Q Dec
buildFunImpl vVars = do
  -- 1. Build the pattern: (v1, v2, ...)
  let pat = tupP (map varP vVars)

  -- 2. Build the body: STupleCons v1 (STupleCons v2 (... STupleNil))
  let body = foldr (buildCons . varE) [e| STupleNil |] vVars

  -- 3. Build the function declaration
  funD 'toSParamTuple [clause [pat] (normalB body) []]

-- | Helper to build: STupleCons v acc
buildCons :: Q Exp -> Q Exp -> Q Exp
buildCons v acc = [e| STupleCons $v $acc |]