{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}

module Lith.Demand.Compute
  ( computeFunctionDeclaration
  , computeAnnoTerm
  ) where

import Prelude hiding (id)

import Data.Map.Strict (Map)

import Control.Applicative (liftA3)
import Data.Foldable (foldl')
import Identifier (Identifier,IdentifierSet)
import FieldIdentifier (FieldIdentifier)

import qualified Identifier
import qualified Lith.Demand.Analyzed as Out
import qualified Lith.Demand.Unanalyzed as In
import qualified FieldIdentifierShortText

computeFunctionDeclaration :: In.FunctionDeclaration -> Out.FunctionDeclaration
computeFunctionDeclaration In.FunctionDeclaration{name,id,argLength,argTypes,argNames,argIds,resType,definition} = Out.FunctionDeclaration
  { name
  , id
  , argLength
  , argTypes
  , argNames
  , argIds
  , resType
  , definition=computeAnnoTerm definition
  }

computeAnnoTerm :: In.AnnoTerm -> Out.AnnoTerm
computeAnnoTerm = goAnnoTerm

goAnnoTerm :: In.AnnoTerm -> Out.AnnoTerm
goAnnoTerm In.AnnoTerm{typ,name,term} =
  let Result anno term' = goTerm term
   in Out.AnnoTerm{typ,anno,name,term=term'}

data Result a = Result !IdentifierSet a

goTerm :: In.Term -> Result Out.Term
goTerm = \case
  In.Var ident v ->
    Result (Identifier.singleton ident) (Out.Var ident v)
  In.LetMany bnds expr ->
    let bnds' = fmap goBinding bnds
        expr'@Out.AnnoTerm{anno=exprVars} = goAnnoTerm expr
        allVars = foldl'
          (\acc Out.Binding{expr=Out.AnnoTerm{anno=vars}} ->
            Identifier.union acc vars
          ) exprVars bnds'
     in Result allVars (Out.LetMany bnds' expr')
  In.Literal lit ->
    let Result vars lit' = goLit lit
     in Result vars (Out.Literal lit')
  In.CaseBool scrutinee onTrue onFalse ->
    let scrutinee'@Out.AnnoTerm{anno=scrutVars} = goAnnoTerm scrutinee
        onTrue'@Out.AnnoTerm{anno=onTrueVars} = goAnnoTerm onTrue
        onFalse'@Out.AnnoTerm{anno=onFalseVars} = goAnnoTerm onFalse
     in Result
          (scrutVars <> onTrueVars <> onFalseVars)
          (Out.CaseBool scrutinee' onTrue' onFalse')
  In.ApplyPrimitive typeArity termArity op typeArgs args ->
    let args' = fmap goAnnoTerm args
        allVars = foldMap (\Out.AnnoTerm{anno=vars} -> vars) args'
     in Result allVars
          (Out.ApplyPrimitive typeArity termArity op typeArgs args')
  In.Project expr field ->
    let expr'@Out.AnnoTerm{anno=vars} = goAnnoTerm expr
     in Result vars (Out.Project expr' field)

goBinding :: In.Binding -> Out.Binding
goBinding In.Binding{id,name,expr} =
  let expr' = goAnnoTerm expr
   in Out.Binding{id,name,expr=expr'}

goLit :: In.Lit -> Result Out.Lit
goLit = \case
  In.LitInt i -> Result mempty (Out.LitInt i)
  In.LitBits w -> Result mempty (Out.LitBits w)
  In.LitTrue ->Result mempty Out.LitTrue
  In.LitFalse ->Result mempty Out.LitFalse
  In.LitArray ty xs ->
    let xs' = fmap goAnnoTerm xs
        fullAnno = foldMap (\Out.AnnoTerm{anno=vars} -> vars) xs'
     in Result fullAnno (Out.LitArray ty xs')
-- x   In.LitCon () con ty args -> do
-- x     args' <- traverse resolveConstructionField args
-- x     case Map.lookup con ?ctx of
-- x       Nothing -> failure "Constructor does not exist"
-- x       Just conId -> do
-- x         ty' <- (traverse.traverse) resolveType ty
-- x         pure (Out.LitCon conId con ty' args')
-- x 
-- x resolveConstructionField :: In.ConstructionField -> Out.ConstructionField
-- x resolveConstructionField In.ConstructionField{name,expr} = do
-- x   expr' <- resolveAnnoTerm expr
-- x   pure Out.ConstructionField{name,expr=expr'}
