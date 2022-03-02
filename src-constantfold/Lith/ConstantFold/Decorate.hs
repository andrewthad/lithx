{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}

module Lith.ConstantFold.Decorate
  ( decorateFunctionDeclaration
  ) where

import Prelude hiding (id,length)

import Data.Foldable (foldl')
import Identifier (Identifier,IdentifierMap)
import Shape (Shape(..),Metadata(..),Alias(..),removeFromContext)
import Data.Maybe (fromMaybe)
import Data.IntMap.Strict (IntMap)
import Vector.Boxed (Vector)

import qualified Identifier
import qualified Arithmetic.Nat as Nat
import qualified Arithmetic.Lt as Lt
import qualified Data.IntMap.Strict as IntMap
import qualified Lith.ConstantFold.Decorated as Out
import qualified Lith.ConstantFold.Unanalyzed as In
import qualified Lith.Primitive as Prim
import qualified Vector.Boxed as Vector

-- Decorate does not ever fail. If a variable is missing, we just say that
-- we do not know anything about its value. The malformed context should get
-- caught elsewhere in the pipeline, and it is nice to not have to plumb
-- everything through a failure monad.
--
-- Decoration currently provides information about let bindings. It does
-- not inform join points about the arguments they are passed. It would
-- be a good idea to support this.
decorateFunctionDeclaration :: In.FunctionDeclaration -> Out.FunctionDeclaration
decorateFunctionDeclaration In.FunctionDeclaration{name,id,argLength,argTypes,argNames,argIds,resType,definition} = Out.FunctionDeclaration
  { name
  , id
  , argLength
  , argTypes
  , argNames
  , argIds
  , resType
    -- TODO: We should not supply the empty map here, but it happens to
    -- work out because we do not error on missing var identifiers.
  , definition=goAnnoTerm Identifier.empty definition
  }

goDecls ::
     IdentifierMap Metadata
  -> In.LoopDeclarations input output st monoid
  -> Out.LoopDeclarations input output st monoid
goDecls ctx In.LoopDeclarations{length,inputArrays,initialStates,outputArrays} =
  Out.LoopDeclarations
    { length = goAnnoTerm ctx length
    , inputArrays = fmap
        (\In.OrderedExpr{order,expr} -> Out.OrderedExpr
          { expr = goAnnoTerm ctx expr
          , order
          }
        ) inputArrays
    , initialStates = fmap (goAnnoTerm ctx) initialStates
    , outputArrays
    , monoids = ()
    }

goBody ::
     IdentifierMap Metadata
  -> In.LoopBody input st
  -> Out.LoopBody input st
goBody ctx In.LoopBody{indexName,indexId,inputArrays,state,expr} = Out.LoopBody
  { indexName
  , indexId
  , inputArrays
  , state
  , expr = goAnnoTerm ctx expr
  }

goEpilogue ::
     IdentifierMap Metadata
  -> In.LoopEpilogue st output monoid
  -> Out.LoopEpilogue st output monoid
goEpilogue ctx In.LoopEpilogue{state,outputArrays,expr} = Out.LoopEpilogue
  { state
  , outputArrays
  , monoids = ()
  , expr = goAnnoTerm ctx expr
  }

removeNamedIdentifiersFromContext :: Vector n Out.NamedIdentifier -> Metadata -> Metadata
removeNamedIdentifiersFromContext !v !m0 = foldl'
  (\acc Out.NamedIdentifier{id} -> removeFromContext id acc
  ) m0 v

-- Don't forgot to add monoid to this once monoids are supported.
removeEpilogueContext :: Out.LoopEpilogue st output monoid -> Metadata -> Metadata
removeEpilogueContext Out.LoopEpilogue{state,outputArrays,monoids=()} meta =
    removeNamedIdentifiersFromContext outputArrays
  $ removeNamedIdentifiersFromContext state
  $ meta

goAnnoTerm :: IdentifierMap Metadata -> In.AnnoTerm -> Out.AnnoTerm
goAnnoTerm ctx In.AnnoTerm{typ,name,term} = case term of
  In.Continue loopId loopName outSt outElems outMonoids -> Out.AnnoTerm
    { typ
    , name
      -- One annotation worth considering is tracking if an expression
      -- always jumps to the same place. For now, I'm ignoring this.
    , anno = Metadata
      { alias = Nothing
      , shape = ShapeUnknown
      }
    , term = Out.Continue loopId loopName
      (fmap (goAnnoTerm ctx) outSt)
      (fmap (goAnnoTerm ctx) outElems)
      (fmap (goAnnoTerm ctx) outMonoids)
    }
  In.For loopId loopName inSz outSz stSz decls body epilogue ->
    let decls' = goDecls ctx decls
        body' = goBody ctx body
        epilogue'@Out.LoopEpilogue{expr=Out.AnnoTerm{anno=epilogueAnno}} = goEpilogue ctx epilogue
        prunedEpilogueAnno = removeEpilogueContext epilogue' epilogueAnno
     in Out.AnnoTerm
          { typ
          , name
          , term = Out.For loopId loopName inSz outSz stSz decls' body' epilogue'
          , anno = prunedEpilogueAnno
          }
  In.Literal lit -> case lit of
    In.LitInt i -> Out.AnnoTerm
      { typ
      , name
      , term = Out.Literal (Out.LitInt i)
      , anno = Metadata {alias = Nothing, shape = ShapeInt i}
      }
  In.Var ident v -> Out.AnnoTerm
    { typ
    , name
    , term = Out.Var ident v
    , anno = case Identifier.lookup ident ctx of
        -- Propogate alias information when possible.
        Just m@Metadata{alias=oldAlias,shape} -> case oldAlias of
          Nothing -> Metadata
            { alias = Just Alias{id=ident,name=v}
            , shape = shape
            }
          Just{} -> m
        Nothing -> Metadata
          { alias = Just Alias{id=ident,name=v}
          , shape = ShapeUnknown
          }
    }
  In.ApplyPrimitive typeArity termArity op typeArgs args ->
    let args' = fmap (goAnnoTerm ctx) args
     in Out.AnnoTerm
        { typ
        , name
        , term = Out.ApplyPrimitive typeArity termArity op typeArgs args'
        , anno = case op of
            Prim.ArrayReplicate
              | Out.AnnoTerm{anno=Metadata{shape=ShapeInt len}} <-
                  Vector.index Lt.constant args' Nat.zero
              , len >= 0
              , len < 256 -- do not create this annotation for giant arrays 
              , Out.AnnoTerm{anno=elAnno} <- Vector.index Lt.constant args' Nat.one
                -> Metadata{alias=Nothing, shape=ShapeArray (buildConstantIntMap (fromIntegral len) elAnno)}
            _ -> Metadata{alias=Nothing, shape=ShapeUnknown}
        }
  In.CaseBool scrut onTrue onFalse -> Out.AnnoTerm
    { typ
    , name
    , term = Out.CaseBool (goAnnoTerm ctx scrut) (goAnnoTerm ctx onTrue) (goAnnoTerm ctx onFalse)
    , anno = Metadata{alias=Nothing, shape=ShapeUnknown} -- TODO: union annotations
    }
  In.LetMany bnds expr ->
    let bnds' = fmap (goBinding ctx) bnds
        ctx' = foldl'
          (\acc Out.Binding{id,expr=Out.AnnoTerm{anno}} -> Identifier.insert id anno acc
          ) ctx bnds'
        expr'@Out.AnnoTerm{anno=exprAnno} = goAnnoTerm ctx' expr
        term' = Out.LetMany bnds' expr'
     in Out.AnnoTerm{typ,name,anno=exprAnno,term=term'}

-- lower bound is understood to be zero
buildConstantIntMap ::
     Int -- upper bound (exclusive)
  -> a
  -> IntMap a
buildConstantIntMap !hi e = go IntMap.empty hi where
  go !acc !ix = case ix of
    (-1) -> acc
    _ -> go (IntMap.insert ix e acc) (ix - 1)

goBinding :: IdentifierMap Metadata -> In.Binding -> Out.Binding
goBinding ctx In.Binding{id,name,expr} = Out.Binding
  { id
  , name
  , expr=goAnnoTerm ctx expr
  }
