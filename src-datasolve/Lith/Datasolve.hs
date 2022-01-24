{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language ImplicitParams #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}

module Lith.Datasolve
  ( resolveAnnoTerm
  , resolveTerm
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

resolveAnnoTerm :: (?ctx :: Map ShortText ConIdentifier)
  => In.AnnoTerm -> M Out.AnnoTerm
resolveAnnoTerm In.AnnoTerm{anno,name,term} = do
  term' <- resolveTerm term
  pure Out.AnnoTerm{anno,name,term=term'}

resolveTerm :: (?ctx :: Map ShortText ConIdentifier) => In.Term -> M Out.Term
resolveTerm = \case
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
