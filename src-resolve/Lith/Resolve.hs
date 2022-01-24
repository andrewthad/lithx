{-# language DuplicateRecordFields #-}
{-# language ImplicitParams #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}

module Lith.Resolve
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
import FreshIdentifier (freshIdentifier)

import qualified Data.Map.Strict as Map
import qualified Lith.Resolve.Out as Out
import qualified Lith.Resolve.In as In
import qualified FieldIdentifierShortText

resolveAnnoTerm ::
  Map ShortText Identifier -> In.AnnoTerm -> M Out.AnnoTerm
resolveAnnoTerm ctx In.AnnoTerm{anno,name,term} = do
  term' <- resolveTerm ctx term
  pure Out.AnnoTerm{anno,name,term=term'}

resolveTerm ::
  Map ShortText Identifier -> In.Term -> M Out.Term
resolveTerm ctx = \case
  In.Var () v -> case Map.lookup v ctx of
    Nothing -> failure "Missing variable"
    Just ident -> pure (Out.Var ident v)
  In.Literal lit -> do
    lit' <- resolveLit ctx lit
    pure (Out.Literal lit')
  In.ApplyPrimitive typeArity termArity op typeArgs args -> do
    args' <- traverse (resolveAnnoTerm ctx) args
    pure (Out.ApplyPrimitive typeArity termArity op typeArgs args')
  In.LetMany bnds expr -> do
    bnds' <- traverse (resolveBinding ctx) bnds
    ctx' <- foldlM
      (\acc Out.Binding{id,name} -> case Map.lookup name acc of
        Just _ -> failure "Shadowed binder"
        Nothing -> pure (Map.insert name id acc)
      ) ctx bnds'
    expr' <- resolveAnnoTerm ctx' expr
    pure (Out.LetMany bnds' expr')
  In.CaseBool scrutinee onTrue onFalse -> liftA3 Out.CaseBool
    (resolveAnnoTerm ctx scrutinee)
    (resolveAnnoTerm ctx onTrue)
    (resolveAnnoTerm ctx onFalse)
  In.Project expr field -> do
    expr' <- resolveAnnoTerm ctx expr
    pure (Out.Project expr' field)

-- The id is not present on syntax terms.
resolveBinding :: 
  Map ShortText Identifier -> In.Binding -> M Out.Binding
resolveBinding ctx In.Binding{id=(),name,expr} = do
  id <- freshIdentifier
  expr' <- resolveAnnoTerm ctx expr
  pure Out.Binding{id,name,expr=expr'}

resolveLit ::
  Map ShortText Identifier -> In.Lit -> M Out.Lit
resolveLit ctx = \case
  In.LitInt i -> pure (Out.LitInt i)
  In.LitBits w -> pure (Out.LitBits w)
  In.LitTrue -> pure Out.LitTrue
  In.LitFalse -> pure Out.LitFalse
  In.LitArray ty xs -> do
    xs' <- traverse (resolveAnnoTerm ctx) xs
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
  expr' <- resolveAnnoTerm ctx expr
  pure Out.ConstructionField{name,expr=expr'}
