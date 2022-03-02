{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language GADTs #-}
{-# language ImplicitParams #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}

module Lith.Typesynth
  ( constantFoldFunctionDeclaration
  , constantFoldAnnoTerm
  ) where

import Prelude hiding (id)

import Control.Monad (when)
import Data.Foldable (foldlM,for_)
import Identifier (IdentifierMap)
import ConIdentifier (ConIdentifier,ConIdentifierMap)
import FieldIdentifier (FieldCollection)
import FunctionIdentifier (FunctionIdentifierMap)
import Machine (M,failure)
import Lith.Type (Type)
import Data.Functor.Identity (Identity(Identity))
import TyVarIdentifier (TyVarIdentifier)
import Data.Primitive (Array)
import Arithmetic.Nat ((=?),(<?))

import qualified Arithmetic.Lt as Lt
import qualified Arithmetic.Nat as Nat
import qualified Arithmetic.Types as Arithmetic
import qualified Lith.Typesynth.Out as Out
import qualified Lith.Typesynth.In as In
import qualified Lith.Primitive as Prim
import qualified Identifier
import qualified ConIdentifier
import qualified FunctionIdentifier
import qualified Lith.Type as Type
import qualified Vector.Boxed as Boxed
import qualified FieldIdentifier

constantFoldFunctionDeclaration :: In.FunctionDeclaration -> M Out.FunctionDeclaration
constantFoldFunctionDeclaration In.FunctionDeclaration{name,id,argLength,argTypes,argNames,argIds,resType,definition} = do
  definition' <-
    let ?funcs = funcsIn
        ?dcons = dconsIn
     in constantFoldAnnoTerm (Boxed.foldrZipWith Identifier.insert Identifier.empty argLength argIds argTypes) definition
  pure Out.FunctionDeclaration
    { name
    , id
    , argLength
    , argTypes
    , argNames
    , argIds
    , resType
    , definition=definition'
    }

typesynthAnnoTerm ::
     ( ?funcs :: FunctionIdentifierMap FunctionDescription
     , ?dcons :: ConIdentifierMap ConDescription
     )
  => IdentifierMap Type -> In.AnnoTerm -> M Out.AnnoTerm
typesynthAnnoTerm ctx In.AnnoTerm{name,anno,term=term0} = case term0 of
  In.Var v varName -> case Identifier.lookup v ctx of
    Nothing -> failure "Missing variable"
    Just ty -> do
      let term = Out.Var v varName
      pure Out.AnnoTerm{typ=ty,anno,name,term}
  In.Literal lit -> do
    AnnoLit{typ,lit=lit'} <- typesynthLit ctx lit
    let term = Out.Literal lit'
    pure Out.AnnoTerm{typ,anno,name,term}
  In.LetMany bnds expr -> do
    bnds' <- traverse (typesynthBinding ctx) bnds
    ctx' <- foldlM
      (\acc Out.Binding{id,expr=Out.AnnoTerm{typ}} ->
        case Identifier.lookup id acc of
          Just _ -> failure "Shadowed binder"
          Nothing -> pure (Identifier.insert id typ acc)
      ) ctx bnds'
    expr'@Out.AnnoTerm{typ=exprAnno} <- typesynthAnnoTerm ctx' expr
    let term = Out.LetMany bnds' expr'
    pure (Out.AnnoTerm{typ=exprAnno,anno,name,term})
  In.Apply funcId (Identity tyArgs) args0 -> do
    args <- traverse (typesynthAnnoTerm ctx) args0
    FunctionDescription{tyVars,argTypes,resultType} <- case FunctionIdentifier.lookup funcId ?funcs of
      Nothing -> failure "Function does not exist"
      Just descr -> pure descr
    Boxed.with tyVars $ \tyVars' -> Boxed.with tyArgs $ \tyArgs' -> Boxed.with argTypes $ \argTypes' -> Boxed.with args $ \args' -> do
      let !tyVarsLen = Boxed.length tyVars'
      let !tyArgsLen = Boxed.length tyArgs'
      eqTyVars <- case tyArgsLen =? tyVarsLen of
        Nothing -> failure "Wrong number of type arguments for function application"
        Just eq -> pure eq
      let tyArgs'' = Boxed.substitute eqTyVars tyArgs'
      expectedTypes <- traverse (substituteType tyVarsLen tyVars' tyArgs'') argTypes'
      let !expectedTypesLen = Boxed.length expectedTypes
      let !argsLen = Boxed.length args'
      eqArgs <- case expectedTypesLen =? argsLen of
        Nothing -> failure "Wrong number of arguments for function application"
        Just eq -> pure eq
      let expectedTypes' = Boxed.substitute eqArgs expectedTypes
      Boxed.zipM_
        (\Out.AnnoTerm{typ=a} b -> when (a /= b) (failure "Function argument had wrong type")
        ) argsLen args' expectedTypes'
      resultType' <- substituteType tyVarsLen tyVars' tyArgs'' resultType
      let term = Out.Apply funcId (Identity tyArgs) args
      pure (Out.AnnoTerm{typ=resultType',anno,name,term})
  In.CaseBool scrutinee onTrue onFalse -> do
    scrutinee'@Out.AnnoTerm{typ=scrutTy} <- typesynthAnnoTerm ctx scrutinee
    case scrutTy of
      Type.Bool -> pure ()
      _ -> failure "Cannot perform case-bool on non boolean type"
    onTrue'@Out.AnnoTerm{typ=onTrueTy} <- typesynthAnnoTerm ctx onTrue
    onFalse'@Out.AnnoTerm{typ=onFalseTy} <- typesynthAnnoTerm ctx onFalse
    when (onTrueTy /= onFalseTy)
      (failure "Arms of case-bool did not have same type")
    let term = Out.CaseBool scrutinee' onTrue' onFalse'
    pure Out.AnnoTerm{typ=onTrueTy,anno,name,term}
  In.ApplyPrimitive typeArity termArity op (Identity typeArgs) args -> do
    args' <- traverse (typesynthAnnoTerm ctx) args
    outTy <- case op of
      Prim.Add
        | Out.AnnoTerm{typ=a} <- Boxed.index Lt.constant args' Nat.zero
        , Out.AnnoTerm{typ=b} <- Boxed.index Lt.constant args' Nat.one
          -> do
            case a of
              Type.Int -> pure ()
              _ -> failure "Expected first argument of add to be Int"
            case b of
              Type.Int -> pure ()
              _ -> failure "Expected second argument of add to be Int"
            pure Type.Int
      Prim.Multiply
        | Out.AnnoTerm{typ=a} <- Boxed.index Lt.constant args' Nat.zero
        , Out.AnnoTerm{typ=b} <- Boxed.index Lt.constant args' Nat.one
          -> do
            case a of
              Type.Int -> pure ()
              _ -> failure "Expected first argument of multiply to be Int"
            case b of
              Type.Int -> pure ()
              _ -> failure "Expected second argument of multiply to be Int"
            pure Type.Int
      Prim.Negate
        | Out.AnnoTerm{typ=a} <- Boxed.index Lt.constant args' Nat.zero
          -> do
            case a of
              Type.Int -> pure ()
              _ -> failure "Expected first argument of add to be Int"
            pure Type.Int
      Prim.ArrayIndex
        | elemTy <- Boxed.index Lt.constant typeArgs Nat.zero
        , Out.AnnoTerm{typ=arrTy} <- Boxed.index Lt.constant args' Nat.zero
        , Out.AnnoTerm{typ=ixTy} <- Boxed.index Lt.constant args' Nat.one
          -> do
            case arrTy of
              Type.Array arrElemTy -> when (elemTy /= arrElemTy)
                (failure "First argument to arrayIndex was array of wrong type") 
              _ -> failure "First argument to arrayIndex was not array"
            case ixTy of
              Type.Int -> pure ()
              _ -> failure "Expected second argument of arrayIndex to be Int"
            pure elemTy
    let term = Out.ApplyPrimitive typeArity termArity op (Identity typeArgs) args'
    pure Out.AnnoTerm{typ=outTy,anno,name,term}
  In.Project expr field -> do
    expr'@Out.AnnoTerm{typ=exprTy} <- typesynthAnnoTerm ctx expr
    case exprTy of
      Type.Box conId _ tyArgs -> case ConIdentifier.lookup conId ?dcons of
        Nothing -> failure "Data constructor missing during projection"
        Just ConDescription{tyVars,argTypes} ->
          case FieldIdentifier.lookup field argTypes of
            Nothing -> failure "Projected field that is not part of type"
            Just rawTy -> do
              Boxed.with tyVars $ \tyVars' -> Boxed.with tyArgs $ \tyArgs' -> do
                let !tyVarsLen = Boxed.length tyVars'
                let !tyArgsLen = Boxed.length tyArgs'
                eqTyVars <- case tyArgsLen =? tyVarsLen of
                  Nothing -> failure "Wrong number of type arguments for data constructor during projection"
                  Just eq -> pure eq
                let tyArgs'' = Boxed.substitute eqTyVars tyArgs'
                ty <- substituteType tyVarsLen tyVars' tyArgs'' rawTy
                let term = Out.Project expr' field
                pure Out.AnnoTerm{typ=ty,anno,name,term}
      _ -> failure "Cannot project field from non-boxed type"

-- The id is not present on syntax terms.
typesynthBinding ::
     ( ?funcs :: FunctionIdentifierMap FunctionDescription
     , ?dcons :: ConIdentifierMap ConDescription
     )
  => IdentifierMap Type -> In.Binding -> M Out.Binding
typesynthBinding ctx In.Binding{id,name,expr} = do
  expr' <- typesynthAnnoTerm ctx expr
  pure Out.Binding{id,name,expr=expr'}

substituteType ::
     Arithmetic.Nat n
  -> Boxed.Vector n TyVarIdentifier -- variable names
  -> Boxed.Vector n Type -- substitution of each variable
  -> Type -- type in which substitution is performed
  -> M Type
substituteType !n vars args ty0 = case ty0 of
  Type.Int -> pure Type.Int
  Type.Bits -> pure Type.Bits
  Type.Bool -> pure Type.Bool
  Type.Array ty -> do
    ty' <- substituteType n vars args ty
    pure (Type.Array ty')
  Type.TyVar var -> do
    sub <- findSubstitution n var vars args
    pure sub
  Type.Box conId name tyArgs -> do
    tyArgs' <- traverse (substituteType n vars args) tyArgs
    pure (Type.Box conId name tyArgs')

findSubstitution ::
     Arithmetic.Nat n
  -> TyVarIdentifier
  -> Boxed.Vector n TyVarIdentifier
  -> Boxed.Vector n Type
  -> M Type
findSubstitution !n !var vars args = go Nat.zero
  where
  go :: Arithmetic.Nat m -> M Type
  go !m = case m <? n of
    Nothing -> failure "Type variable not found in substitutions"
    Just lt -> do
      let ident = Boxed.index lt vars m
      if ident == var
        then pure (Boxed.index lt args m)
        else go (Nat.succ m)

-- Used internally as result of typesynthLit.
data AnnoLit = AnnoLit
  { typ :: Type
  , lit :: Out.Lit
  }

typesynthLit ::
     ( ?funcs :: FunctionIdentifierMap FunctionDescription
     , ?dcons :: ConIdentifierMap ConDescription
     )
  => IdentifierMap Type -> In.Lit -> M AnnoLit
typesynthLit ctx = \case
  In.LitInt i -> pure (AnnoLit Type.Int (Out.LitInt i))
  In.LitBits w -> pure (AnnoLit Type.Bits (Out.LitBits w))
  In.LitTrue -> pure (AnnoLit Type.Bool Out.LitTrue)
  In.LitFalse -> pure (AnnoLit Type.Bool Out.LitFalse)
  In.LitArray (Identity ty) xs -> do
    xs' <- traverse (typesynthAnnoTerm ctx) xs
    for_ xs' $ \Out.AnnoTerm{typ} ->
      when (typ /= ty) (failure "Array element had wrong type")
    pure (AnnoLit (Type.Array ty) (Out.LitArray (Identity ty) xs'))
  In.LitCon con conName (Identity tyArgs) args0 -> do
    -- Similar to function application, but the arguments are in
    -- a map with keys rather than an array. That is, is in by name
    -- instead of by position.
    args <- traverse (typesynthConstructionField ctx) args0
    ConDescription{tyVars,argTypes} <- case ConIdentifier.lookup con ?dcons of
      Nothing -> failure "Data constructor does not exist"
      Just descr -> pure descr
    Boxed.with tyVars $ \tyVars' -> Boxed.with tyArgs $ \tyArgs' -> do
      let !tyVarsLen = Boxed.length tyVars'
      let !tyArgsLen = Boxed.length tyArgs'
      eqTyVars <- case tyArgsLen =? tyVarsLen of
        Nothing -> failure "Wrong number of type arguments for data constructor during construction"
        Just eq -> pure eq
      let tyArgs'' = Boxed.substitute eqTyVars tyArgs'
      expectedTypes <- traverse (substituteType tyVarsLen tyVars' tyArgs'') argTypes
      when (not (FieldIdentifier.sameKeys args expectedTypes))
        (failure "Data constructor had unexpected keys or was missing keys")
      FieldIdentifier.zipM_
        (\Out.ConstructionField{expr=Out.AnnoTerm{typ=a}} b -> when (a /= b)
          (failure "Data constructor argument had wrong type")
        ) args expectedTypes
    let lit = Out.LitCon con conName (Identity tyArgs) args
    pure AnnoLit{typ=Type.Box con conName tyArgs,lit}

typesynthConstructionField ::
     ( ?funcs :: FunctionIdentifierMap FunctionDescription
     , ?dcons :: ConIdentifierMap ConDescription
     )
  => IdentifierMap Type
  -> In.ConstructionField
  -> M Out.ConstructionField
typesynthConstructionField ctx In.ConstructionField{name,expr} = do
  expr' <- typesynthAnnoTerm ctx expr
  pure Out.ConstructionField{name,expr=expr'}

