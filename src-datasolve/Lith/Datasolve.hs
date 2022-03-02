{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language ImplicitParams #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}

module Lith.Datasolve
  ( datasolveFunctionDeclaration
  ) where

import Prelude hiding (id)

import Data.Map.Strict (Map)

import Control.Applicative (liftA3)
import Data.Foldable (foldlM)
import Data.Text.Short (ShortText)
import Identifier (Identifier)
import ConIdentifier (ConIdentifier)
import FieldIdentifier (FieldIdentifier)
import Machine (M,failure)

import qualified Data.Map.Strict as Map
import qualified Lith.Datasolve.Out as Out
import qualified Lith.Datasolve.In as In
import qualified FieldIdentifierShortText

datasolveFunctionDeclaration :: Map ShortText ConIdentifier -> In.FunctionDeclaration -> M Out.FunctionDeclaration
datasolveFunctionDeclaration m In.FunctionDeclaration{name,id,argLength,argTypes,argNames,argIds,resType,definition} = do
  definition' <- let ?ctx = m in resolveAnnoTerm definition
  argTypes' <- let ?ctx = m in traverse resolveType argTypes
  resType' <- let ?ctx = m in resolveType resType
  pure Out.FunctionDeclaration
    { name
    , id
    , argLength
    , argTypes=argTypes'
    , argNames
    , argIds
    , resType=resType'
    , definition=definition'
    }

resolveAnnoTerm :: (?ctx :: Map ShortText ConIdentifier)
  => In.AnnoTerm -> M Out.AnnoTerm
resolveAnnoTerm In.AnnoTerm{typ,anno,name,term} = do
  term' <- resolveTerm term
  pure Out.AnnoTerm{typ,anno,name,term=term'}

resolveLoopDeclarations :: (?ctx :: Map ShortText ConIdentifier)
  => In.LoopDeclarations input output st monoids
  -> M (Out.LoopDeclarations input output st monoids)
resolveLoopDeclarations In.LoopDeclarations{length,inputArrays,initialStates,outputArrays} = do
  length' <- resolveAnnoTerm length
  inputArrays' <- traverse
    (\In.OrderedExpr{order,expr} -> do
      expr' <- resolveAnnoTerm expr
      pure Out.OrderedExpr{order,expr=expr'}
    ) inputArrays
  outputArrays' <- traverse
    (\In.LoopArray{order,typ} -> do
      typ' <- resolveType typ
      pure Out.LoopArray{order,typ=typ'}
    ) outputArrays
  initialStates' <- traverse resolveAnnoTerm initialStates
  pure Out.LoopDeclarations
    { length = length'
    , inputArrays = inputArrays'
    , initialStates = initialStates'
    , outputArrays = outputArrays'
    , monoids = ()
    }

resolveLoopBody :: (?ctx :: Map ShortText ConIdentifier)
  => In.LoopBody input st
  -> M (Out.LoopBody input st)
resolveLoopBody In.LoopBody{indexName,indexId,inputArrays,state,expr} = do
  expr' <- resolveAnnoTerm expr
  pure Out.LoopBody
    { indexName
    , indexId
    , inputArrays
    , state
    , expr = expr'
    }

resolveLoopEpilogue :: (?ctx :: Map ShortText ConIdentifier)
  => In.LoopEpilogue st output monoid
  -> M (Out.LoopEpilogue st output monoid)
resolveLoopEpilogue In.LoopEpilogue{state,outputArrays,expr} = do
  expr' <- resolveAnnoTerm expr
  pure Out.LoopEpilogue
    { state
    , outputArrays
    , monoids = ()
    , expr = expr'
    }

resolveTerm :: (?ctx :: Map ShortText ConIdentifier) => In.Term -> M Out.Term
resolveTerm = \case
  In.For ident name inputSz outputSz stateSz decls body epilogue -> do
    decls' <- resolveLoopDeclarations decls
    body' <- resolveLoopBody body
    epilogue' <- resolveLoopEpilogue epilogue
    pure (Out.For ident name inputSz outputSz stateSz decls' body' epilogue')
  In.Continue ident name state elems monoids -> do
    state' <- traverse resolveAnnoTerm state
    elems' <- traverse resolveAnnoTerm elems
    monoids' <- traverse resolveAnnoTerm monoids
    pure (Out.Continue ident name state' elems' monoids')
  In.Var ident v -> pure (Out.Var ident v)
  In.Literal lit -> do
    lit' <- resolveLit lit
    pure (Out.Literal lit')
  In.ApplyPrimitive typeArity termArity op typeArgs args -> do
    args' <- traverse resolveAnnoTerm args
    typeArgs' <- traverse (traverse resolveType) typeArgs
    pure (Out.ApplyPrimitive typeArity termArity op typeArgs' args')
  In.LetMany bnds expr -> do
    bnds' <- traverse resolveBinding bnds
    expr' <- resolveAnnoTerm expr
    pure (Out.LetMany bnds' expr')
  In.CaseBool scrutinee onTrue onFalse -> liftA3 Out.CaseBool
    (resolveAnnoTerm scrutinee)
    (resolveAnnoTerm onTrue)
    (resolveAnnoTerm onFalse)
  In.Project expr field -> do
    expr' <- resolveAnnoTerm expr
    pure (Out.Project expr' field)

-- The id is not present on syntax terms.
resolveBinding :: (?ctx :: Map ShortText ConIdentifier)
  => In.Binding -> M Out.Binding
resolveBinding In.Binding{id,name,expr} = do
  expr' <- resolveAnnoTerm expr
  pure Out.Binding{id,name,expr=expr'}

resolveLit :: (?ctx :: Map ShortText ConIdentifier)
  => In.Lit -> M Out.Lit
resolveLit = \case
  In.LitInt i -> pure (Out.LitInt i)
  In.LitBits w -> pure (Out.LitBits w)
  In.LitTrue -> pure Out.LitTrue
  In.LitFalse -> pure Out.LitFalse
  In.LitArray ty xs -> do
    xs' <- traverse resolveAnnoTerm xs
    ty' <- traverse resolveType ty
    pure (Out.LitArray ty' xs')
  In.LitCon () con ty args -> do
    args' <- traverse resolveConstructionField args
    case Map.lookup con ?ctx of
      Nothing -> failure "Constructor does not exist"
      Just conId -> do
        ty' <- (traverse.traverse) resolveType ty
        pure (Out.LitCon conId con ty' args')

resolveConstructionField ::
     (?ctx :: Map ShortText ConIdentifier)
  => In.ConstructionField
  -> M Out.ConstructionField
resolveConstructionField In.ConstructionField{name,expr} = do
  expr' <- resolveAnnoTerm expr
  pure Out.ConstructionField{name,expr=expr'}

resolveType :: (?ctx :: Map ShortText ConIdentifier)
  => In.Type -> M Out.Type
resolveType = \case
  In.Int -> pure (Out.Int)
  In.Bits -> pure (Out.Bits)
  In.Bool -> pure (Out.Bool)
  In.Array ty -> fmap Out.Array (resolveType ty)
  In.Box () con args -> case Map.lookup con ?ctx of
    Nothing -> failure "Constructor in box type does not exist"
    Just conId -> do
      args' <- traverse resolveType args
      pure (Out.Box conId con args')
  In.TyVar v -> pure (Out.TyVar v)
