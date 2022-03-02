{-# language DataKinds #-}
{-# language LambdaCase #-}
{-# language DuplicateRecordFields #-}
{-# language GADTs #-}
{-# language MultiWayIf #-}
{-# language NamedFieldPuns #-}
{-# language NumericUnderscores #-}
{-# language TypeApplications #-}

module Lith.ConstantFold.Run
  ( run
  ) where

import Prelude hiding (id)
import Data.Foldable (foldl')
import Data.Primitive (SmallArray)
import Arithmetic.Types (Fin(Fin))
import Vector.Boxed (Vector)
import Shape (Metadata(..),Shape(..),Alias(..))

import qualified Arithmetic.Types as Arithmetic
import qualified Arithmetic.Fin as Fin
import qualified Arithmetic.Lt as Lt
import qualified Arithmetic.Nat as Nat
import qualified Data.Primitive.Contiguous as C
import qualified Data.Primitive as PM

import qualified Identifier
import qualified Lith.ConstantFold.Decorated as In
import qualified Lith.ConstantFold.Optimized as Out
import qualified Lith.Primitive as Prim
import qualified Vector.Boxed as Vector
import qualified Data.IntMap.Strict as IntMap

-- | This pass floats let bindings inward, closer to the use site of the
-- binding. This is able to float a binding inward multiple times in the
-- same pass.
run :: In.FunctionDeclaration -> Out.FunctionDeclaration
run In.FunctionDeclaration{name,id,argLength,argTypes,argNames,argIds,resType,definition} =
  Out.FunctionDeclaration
    { name
    , id
    , argLength
    , argTypes
    , argNames
    , argIds
    , resType
    , definition=goAnnoTerm definition
    }

goAnnoTerm :: In.AnnoTerm -> Out.AnnoTerm
goAnnoTerm In.AnnoTerm{typ,name,term,anno=Metadata{shape,alias}} = case shape of
  ShapeInt i -> Out.AnnoTerm{typ,name,anno=(),term=Out.Literal (Out.LitInt i)}
  _ -> case alias of
    Nothing -> Out.AnnoTerm{typ,name,anno=(),term=goTerm term}
    Just Alias{id,name} -> Out.AnnoTerm{typ,name,anno=(),term=Out.Var id name}

goLit :: In.Lit -> Out.Lit
goLit = \case
  In.LitInt i -> Out.LitInt i

goDecls ::
     In.LoopDeclarations input output st monoid
  -> Out.LoopDeclarations input output st monoid
goDecls In.LoopDeclarations{length,inputArrays,initialStates,outputArrays} =
  Out.LoopDeclarations
    { length = goAnnoTerm length
    , inputArrays = fmap
        (\In.OrderedExpr{order,expr} -> Out.OrderedExpr
          { expr = goAnnoTerm expr
          , order
          }
        ) inputArrays
    , initialStates = fmap goAnnoTerm initialStates
    , outputArrays
    , monoids = ()
    }

goBody ::
     In.LoopBody input st
  -> Out.LoopBody input st
goBody In.LoopBody{indexName,indexId,inputArrays,state,expr} = Out.LoopBody
  { indexName
  , indexId
  , inputArrays
  , state
  , expr = goAnnoTerm expr
  }

goEpilogue ::
     In.LoopEpilogue st output monoid
  -> Out.LoopEpilogue st output monoid
goEpilogue In.LoopEpilogue{state,outputArrays,expr} = Out.LoopEpilogue
  { state
  , outputArrays
  , monoids = ()
  , expr = goAnnoTerm expr
  }


goTerm :: In.Term -> Out.Term
goTerm = \case
  In.For loopId loopName inSz outSz stSz decls body epilogue ->
    let decls' = goDecls decls
        body' = goBody body
        epilogue' = goEpilogue epilogue
     in Out.For loopId loopName inSz outSz stSz decls' body' epilogue'
  In.Continue loopId loopName outSt outElems outMonoids -> Out.Continue loopId loopName
    (fmap goAnnoTerm outSt)
    (fmap goAnnoTerm outElems)
    (fmap goAnnoTerm outMonoids)
  In.Literal lit -> Out.Literal (goLit lit)
  In.Var ident v -> Out.Var ident v
  In.LetMany bnds expr -> Out.LetMany (fmap goBinding bnds) (goAnnoTerm expr)
  In.CaseBool scrut onTrue onFalse ->
    Out.CaseBool (goAnnoTerm scrut) (goAnnoTerm onTrue) (goAnnoTerm onFalse)
  In.ApplyPrimitive typeArity termArity op typeArgs args -> case op of
    Prim.Multiply ->
      let In.AnnoTerm{anno=Metadata{shape=s0},term=t0} = Vector.index Lt.constant args (Nat.constant @0)
          In.AnnoTerm{anno=Metadata{shape=s1},term=t1} = Vector.index Lt.constant args (Nat.constant @1)
       in if | ShapeInt 0 <- s0 -> Out.Literal (Out.LitInt 0)
             | ShapeInt 0 <- s1 -> Out.Literal (Out.LitInt 0)
             | ShapeInt 1 <- s0 -> goTerm t1
             | ShapeInt 1 <- s1 -> goTerm t0
             | ShapeInt n0 <- s0, ShapeInt n1 <- s1 -> Out.Literal (Out.LitInt (n0 * n1))
             | otherwise ->
                 Out.ApplyPrimitive typeArity termArity op typeArgs (fmap goAnnoTerm args)
    Prim.Add ->
      let In.AnnoTerm{anno=Metadata{shape=s0},term=t0} = Vector.index Lt.constant args (Nat.constant @0)
          In.AnnoTerm{anno=Metadata{shape=s1},term=t1} = Vector.index Lt.constant args (Nat.constant @1)
       in if | ShapeInt 0 <- s0 -> goTerm t1
             | ShapeInt 0 <- s1 -> goTerm t0
             | ShapeInt n0 <- s0, ShapeInt n1 <- s1 -> Out.Literal (Out.LitInt (n0 + n1))
             | otherwise ->
                 Out.ApplyPrimitive typeArity termArity op typeArgs (fmap goAnnoTerm args)
    Prim.ArrayIndex ->
      let In.AnnoTerm{anno=Metadata{shape=s0},term=t0} = Vector.index Lt.constant args (Nat.constant @0)
          In.AnnoTerm{anno=Metadata{shape=s1},term=t1} = Vector.index Lt.constant args (Nat.constant @1)
       in if | ShapeArray arr <- s0
             , ShapeInt ix <- s1
             , ix < 0x0FFF_FFFF
             , Just Metadata{shape,alias} <- IntMap.lookup (fromIntegral ix) arr -> case shape of
                 ShapeInt i -> Out.Literal (Out.LitInt i)
                 _ -> case alias of
                   Nothing -> Out.ApplyPrimitive typeArity termArity op typeArgs (fmap goAnnoTerm args)
                   Just Alias{id,name} -> Out.Var id name
             | otherwise ->
                 Out.ApplyPrimitive typeArity termArity op typeArgs (fmap goAnnoTerm args)
    _ -> Out.ApplyPrimitive typeArity termArity op typeArgs (fmap goAnnoTerm args)

goBinding :: In.Binding -> Out.Binding
goBinding In.Binding{id,name,expr} = Out.Binding
  { id
  , name
  , expr=goAnnoTerm expr
  }
