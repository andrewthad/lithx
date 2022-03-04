{-# language DuplicateRecordFields #-}
{-# language NamedFieldPuns #-}
{-# language GADTs #-}

module Lith.Term.Eq
  ( eqFunctionDeclaration
  , eqTerm
  ) where

import Prelude hiding (id)
import Data.Foldable (foldl')
import Data.Primitive (SmallArray,Array)
import Data.Text.Short (ShortText)
import Lith.Primitive (Primitive)
import Arithmetic.Nat ((=?))
import Identifier (Identifier)
import Data.Functor.Classes (liftEq)
import Data.Type.Equality ((:~:)(Refl))

import qualified Arithmetic.Types as Arithmetic
import qualified Lith.Term as In
import qualified Lith.Primitive as Prim
import qualified Vector.Boxed as Vector

eqFunctionDeclaration :: In.FunctionDeclaration -> In.FunctionDeclaration -> Bool
eqFunctionDeclaration
  In.FunctionDeclaration{definition=d0} In.FunctionDeclaration{definition=d1} =
    eqAnnoTerm d0 d1

eqAnnoTerm :: In.AnnoTerm -> In.AnnoTerm -> Bool
eqAnnoTerm In.AnnoTerm{term=t0} In.AnnoTerm{term=t1} = eqTerm t0 t1

eqOrderedExpr :: In.OrderedExpr -> In.OrderedExpr -> Bool
eqOrderedExpr In.OrderedExpr{order=order0,expr=e0} In.OrderedExpr{order=order1,expr=e1} =
  order0 == order1 && eqAnnoTerm e0 e1

-- TODO: maybe compare type as well
eqLoopArray :: In.LoopArray -> In.LoopArray -> Bool
eqLoopArray In.LoopArray{order=order0} In.LoopArray{order=order1} = order0 == order1

eqNamedIdentifier :: In.NamedIdentifier -> In.NamedIdentifier -> Bool
eqNamedIdentifier In.NamedIdentifier{id=id0} In.NamedIdentifier{id=id1} = id0 == id1

eqDecls ::
     In.LoopDeclarations input0 output0 st0 monoid0
  -> In.LoopDeclarations input1 output1 st1 monoid1
  -> Bool
eqDecls
  In.LoopDeclarations{length=len0,inputArrays=in0,initialStates=st0,outputArrays=out0,monoids=m0}
  In.LoopDeclarations{length=len1,inputArrays=in1,initialStates=st1,outputArrays=out1,monoids=m1} =
  eqAnnoTerm len0 len1 &&
  liftEq eqOrderedExpr (Vector.forget in0) (Vector.forget in1) &&
  liftEq eqAnnoTerm (Vector.forget st0) (Vector.forget st1) &&
  liftEq eqLoopArray (Vector.forget out0) (Vector.forget out1) &&
  m0 == m1

eqBody ::
     In.LoopBody st0 output0
  -> In.LoopBody st1 output1
  -> Bool
eqBody
  In.LoopBody{indexId=indexId0,inputArrays=in0,state=st0,expr=expr0}
  In.LoopBody{indexId=indexId1,inputArrays=in1,state=st1,expr=expr1} =
  indexId0 == indexId1 &&
  eqAnnoTerm expr0 expr1 &&
  liftEq eqNamedIdentifier (Vector.forget in0) (Vector.forget in1) &&
  liftEq eqNamedIdentifier (Vector.forget st0) (Vector.forget st1)

eqEpilogue ::
     In.LoopEpilogue st0 output0 monoid0
  -> In.LoopEpilogue st1 output1 monoid1
  -> Bool
eqEpilogue
  In.LoopEpilogue{monoids=monoids0,outputArrays=out0,state=st0,expr=expr0}
  In.LoopEpilogue{monoids=monoids1,outputArrays=out1,state=st1,expr=expr1} =
  monoids0 == monoids1 &&
  eqAnnoTerm expr0 expr1 &&
  liftEq eqNamedIdentifier (Vector.forget out0) (Vector.forget out1) &&
  liftEq eqNamedIdentifier (Vector.forget st0) (Vector.forget st1)

-- TODO: test For construct for equality
eqTerm :: In.Term -> In.Term -> Bool
eqTerm (In.Continue i0 _ st0 el0 md0) (In.Continue i1 _ st1 el1 md1) =
  i0 == i1 &&
  liftEq eqAnnoTerm st0 st1 &&
  liftEq eqAnnoTerm el0 el1 &&
  liftEq eqAnnoTerm md0 md1
eqTerm (In.For loopId0 _ _ _ _ decls0 body0 epilogue0) (In.For loopId1 _ _ _ _ decls1 body1 epilogue1) =
  loopId0 == loopId1 &&
  eqDecls decls0 decls1 &&
  eqBody body0 body1 &&
  eqEpilogue epilogue0 epilogue1
eqTerm (In.Var i0 _) (In.Var i1 _) = i0 == i1
eqTerm (In.Literal lit0) (In.Literal lit1) = eqLit lit0 lit1
eqTerm (In.CaseBool s0 t0 f0) (In.CaseBool s1 t1 f1) =
  eqAnnoTerm s0 s1 && eqAnnoTerm t0 t1 && eqAnnoTerm f0 f1
eqTerm (In.LetMany bnds0 e0) (In.LetMany bnds1 e1) =
  eqAnnoTerm e0 e1 && liftEq eqBinding bnds0 bnds1
eqTerm (In.ApplyPrimitive _ termArity op0 _ terms0) (In.ApplyPrimitive _ _ op1 _ terms1) =
  case Prim.testEquality op0 op1 of
    Just Refl -> Vector.foldrZipWith
      (\x y acc -> eqAnnoTerm x y && acc) True termArity terms0 terms1
    Nothing -> False
eqTerm _ _ = False

eqBinding :: In.Binding -> In.Binding -> Bool
eqBinding In.Binding{id=id0,expr=expr0} In.Binding{id=id1,expr=expr1} =
  id0 == id1 && eqAnnoTerm expr0 expr1

eqLit :: In.Lit -> In.Lit -> Bool
eqLit (In.LitInt i0) (In.LitInt i1) = i0 == i1
eqLit (In.LitBits i0) (In.LitBits i1) = i0 == i1
eqLit In.LitTrue In.LitTrue = True
eqLit In.LitFalse In.LitFalse = True
eqLit _ _ = False
