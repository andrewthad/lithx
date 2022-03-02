{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language ImplicitParams #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}

module Lith.Resolve
  ( resolveFunctionDeclaration
  , resolveAnnoTerm
  , resolveTerm
  ) where

import Prelude hiding (id)

import Data.Map.Strict (Map)

import Control.Applicative (liftA3)
import Control.Monad (when)
import Data.Foldable (foldlM,foldl')
import Data.Maybe (isJust)
import Data.Text.Short (ShortText)
import Identifier (Identifier)
import LoopIdentifier (LoopIdentifier)
import ConIdentifier (ConIdentifier)
import FieldIdentifier (FieldIdentifier)
import Machine (M,failure)
import FreshIdentifier (freshIdentifier,freshLoopIdentifier)
import Vector.Boxed (Vector)

import qualified Data.Map.Strict as Map
import qualified Lith.Resolve.Out as Out
import qualified Lith.Resolve.In as In
import qualified FieldIdentifierShortText

-- Initial context is empty
resolveFunctionDeclaration :: In.FunctionDeclaration -> M Out.FunctionDeclaration
resolveFunctionDeclaration In.FunctionDeclaration{name,id=funcId,argLength,argTypes,argNames,resType,definition} = do
  argPairs <- traverse
    (\name -> do
      ident <- freshIdentifier
      pure (ident,name)
    ) argNames
  let !ctx = foldl'
        (\acc (ident,name) -> Map.insert name ident acc
        ) Map.empty argPairs
  definition' <- resolveAnnoTerm ctx Map.empty definition
  pure Out.FunctionDeclaration
    { name
    , id=funcId
    , argLength
    , argTypes
    , argNames
    , argIds=fmap fst argPairs
    , resType
    , definition=definition'
    }

resolveAnnoTerm ::
     Map ShortText Identifier
  -> Map ShortText LoopIdentifier
  -> In.AnnoTerm
  -> M Out.AnnoTerm
resolveAnnoTerm !ctx !lctx In.AnnoTerm{typ,anno,name,term} = do
  term' <- resolveTerm ctx lctx term
  pure Out.AnnoTerm{typ,anno,name,term=term'}

resolveLoopBody ::
     Map ShortText Identifier
  -> Map ShortText LoopIdentifier
  -> In.LoopBody input st
  -> M (Out.LoopBody input st)
resolveLoopBody ctx0 lctx In.LoopBody{indexName,inputArrays,state,expr} = do
  when (isJust (Map.lookup indexName ctx0)) (failure "Index name shadows")
  indexId <- freshIdentifier
  let ctx1 = Map.insert indexName indexId ctx0
  (ctx2, inputArrays') <- nameIdentifiers ctx1 inputArrays
  (ctx3, state') <- nameIdentifiers ctx2 state
  expr' <- resolveAnnoTerm ctx3 lctx expr
  pure Out.LoopBody
    { indexId
    , indexName
    , inputArrays=inputArrays'
    , state=state'
    , expr=expr'
    }

resolveLoopEpilogue ::
     Map ShortText Identifier
  -> Map ShortText LoopIdentifier
  -> In.LoopEpilogue st output monoid
  -> M (Out.LoopEpilogue st output monoid)
resolveLoopEpilogue ctx0 lctx In.LoopEpilogue{state,outputArrays,expr} = do
  (ctx1, state') <- nameIdentifiers ctx0 state
  (ctx2, outputArrays') <- nameIdentifiers ctx1 outputArrays
  expr' <- resolveAnnoTerm ctx2 lctx expr
  pure Out.LoopEpilogue
    { outputArrays=outputArrays'
    , state=state'
    , monoids=()
    , expr=expr'
    }

resolveLoopDeclarations ::
     Map ShortText Identifier
  -> In.LoopDeclarations input output st monoid
  -> M (Out.LoopDeclarations input output st monoid)
resolveLoopDeclarations !ctx In.LoopDeclarations{length,inputArrays,initialStates,outputArrays} = do
  length' <- resolveAnnoTerm ctx Map.empty length
  inputArrays' <- traverse
    (\In.OrderedExpr{order,expr} -> do
      expr' <- resolveAnnoTerm ctx Map.empty expr
      pure Out.OrderedExpr{order,expr=expr'}
    ) inputArrays
  initialStates' <- traverse (resolveAnnoTerm ctx Map.empty) initialStates
  pure Out.LoopDeclarations
    { length = length'
    , inputArrays = inputArrays'
    , initialStates = initialStates'
    , outputArrays
    , monoids = ()
    }

nameIdentifiers ::
     Map ShortText Identifier
  -> Vector n In.NamedIdentifier
  -> M (Map ShortText Identifier, Vector n Out.NamedIdentifier)
nameIdentifiers !ctx0 v = do
  v' <- traverse
    (\In.NamedIdentifier{name} -> do
      id <- freshIdentifier
      pure Out.NamedIdentifier{id,name}
    ) v
  ctx1 <- foldlM
    (\acc Out.NamedIdentifier{id,name} -> case Map.lookup name acc of
      Just _ -> failure "Name shadowing"
      Nothing -> pure (Map.insert name id acc)
    ) ctx0 v'
  pure (ctx1, v')

resolveTerm ::
     Map ShortText Identifier
  -> Map ShortText LoopIdentifier
  -> In.Term
  -> M Out.Term
resolveTerm ctx lctx = \case
  In.Continue _ name state elems monoids -> case Map.lookup name lctx of
    Nothing -> failure "Missing for-loop name"
    Just ident -> do
      state' <- traverse (resolveAnnoTerm ctx Map.empty) state
      elems' <- traverse (resolveAnnoTerm ctx Map.empty) elems
      monoids' <- traverse (resolveAnnoTerm ctx Map.empty) monoids
      pure (Out.Continue ident name state' elems' monoids')
  In.For () name inputSz outputSz stateSz decls body epilogue ->
    case Map.lookup name lctx of
      Nothing -> do
        ident <- freshLoopIdentifier
        decls' <- resolveLoopDeclarations ctx decls
        body' <- resolveLoopBody ctx (Map.insert name ident lctx) body
        epilogue' <- resolveLoopEpilogue ctx lctx epilogue
        pure (Out.For ident name inputSz outputSz stateSz decls' body' epilogue')
      Just _ -> failure "Shadowed for-loop name"
  In.Var () v -> case Map.lookup v ctx of
    Nothing -> failure "Missing variable"
    Just ident -> pure (Out.Var ident v)
  In.Literal lit -> do
    lit' <- resolveLit ctx lit
    pure (Out.Literal lit')
  In.ApplyPrimitive typeArity termArity op typeArgs args -> do
    args' <- traverse (resolveAnnoTerm ctx Map.empty) args
    pure (Out.ApplyPrimitive typeArity termArity op typeArgs args')
  In.LetMany bnds expr -> do
    bnds' <- traverse (resolveBinding ctx) bnds
    ctx' <- foldlM
      (\acc Out.Binding{id,name} -> case Map.lookup name acc of
        Just _ -> failure "Shadowed binder"
        Nothing -> pure (Map.insert name id acc)
      ) ctx bnds'
    expr' <- resolveAnnoTerm ctx' lctx expr
    pure (Out.LetMany bnds' expr')
  In.CaseBool scrutinee onTrue onFalse -> liftA3 Out.CaseBool
    (resolveAnnoTerm ctx lctx scrutinee)
    (resolveAnnoTerm ctx lctx onTrue)
    (resolveAnnoTerm ctx lctx onFalse)
  In.Project expr field -> do
    expr' <- resolveAnnoTerm ctx lctx expr
    pure (Out.Project expr' field)

-- The id is not present on syntax terms.
resolveBinding :: 
  Map ShortText Identifier -> In.Binding -> M Out.Binding
resolveBinding ctx In.Binding{id=(),name,expr} = do
  id <- freshIdentifier
  expr' <- resolveAnnoTerm ctx Map.empty expr
  pure Out.Binding{id,name,expr=expr'}

resolveLit ::
     Map ShortText Identifier
  -> In.Lit
  -> M Out.Lit
resolveLit ctx = \case
  In.LitInt i -> pure (Out.LitInt i)
  In.LitBits w -> pure (Out.LitBits w)
  In.LitTrue -> pure Out.LitTrue
  In.LitFalse -> pure Out.LitFalse
  In.LitArray ty xs -> do
    xs' <- traverse (resolveAnnoTerm ctx Map.empty) xs
    pure (Out.LitArray ty xs')
  In.LitCon con conName ty args -> do
    args' <- traverse
      (\arg -> do
        resolveConstructionField ctx arg
      ) args
    pure (Out.LitCon con conName ty args')

resolveConstructionField ::
     Map ShortText Identifier
  -> In.ConstructionField
  -> M Out.ConstructionField
resolveConstructionField ctx In.ConstructionField{name,expr} = do
  expr' <- resolveAnnoTerm ctx Map.empty expr
  pure Out.ConstructionField{name,expr=expr'}
