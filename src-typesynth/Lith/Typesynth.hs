{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language GADTs #-}
{-# language ImplicitParams #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}

module Lith.Typesynth
  ( typesynthFunctionDeclaration
  , typesynthAnnoTerm
  ) where

import Prelude hiding (id,length)

import Control.Monad (when)
import Data.Foldable (foldlM,for_)
import Identifier (IdentifierMap)
import LoopIdentifier (LoopIdentifierMap)
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
import qualified Data.Primitive as PM
import qualified Lith.Typesynth.Out as Out
import qualified Lith.Typesynth.In as In
import qualified Lith.Primitive as Prim
import qualified Identifier
import qualified LoopIdentifier
import qualified ConIdentifier
import qualified FunctionIdentifier
import qualified Lith.Type as Type
import qualified Vector.Boxed as Boxed
import qualified FieldIdentifier

-- Probably need to move this out to somewhere with fewer dependencies
-- so that it can be shared more freely.
data FunctionDescription = FunctionDescription
  { tyVars :: !(Array TyVarIdentifier)
  , argTypes :: !(Array Type)
  , resultType :: Type
  }

data ConDescription = ConDescription
  { tyVars :: !(Array TyVarIdentifier)
    -- ^ Variables that need to be instantiated.
  , argTypes :: !(FieldCollection Type)
    -- ^ Types of the fields. These may reference the type variables.
  , parent :: !(Maybe ConIdentifier)
    -- ^ Track the parent constructor so that we can typecheck
    -- case-on-box expressions.
  }

typesynthFunctionDeclaration ::
     FunctionIdentifierMap FunctionDescription
  -> ConIdentifierMap ConDescription
  -> In.FunctionDeclaration
  -> M Out.FunctionDeclaration
typesynthFunctionDeclaration funcsIn dconsIn In.FunctionDeclaration{name,id,argLength,argTypes,argNames,argIds,resType,definition} = do
  definition' <-
    let ?funcs = funcsIn
        ?dcons = dconsIn
     in typesynthAnnoTerm (Boxed.foldrZipWith Identifier.insert Identifier.empty argLength argIds argTypes) LoopIdentifier.empty definition
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

data ContinueExpectation = ContinueExpectation
  { states :: !(Array Type)
  , elems :: !(Array Type)
  , monoids :: !(Array Type)
  }

makeContinueExpectation :: Out.LoopDeclarations input output st monoid -> ContinueExpectation
makeContinueExpectation Out.LoopDeclarations{initialStates,outputArrays} = ContinueExpectation
  { states = fmap (\Out.AnnoTerm{typ} -> typ) (Boxed.forget initialStates)
  , elems = fmap (\Out.LoopArray{typ} -> typ) (Boxed.forget outputArrays)
  , monoids = mempty
  }

getTermType :: Out.AnnoTerm -> Type
getTermType Out.AnnoTerm{typ} = typ

addLoopBodyContext ::
     Arithmetic.Nat input
  -> Arithmetic.Nat st
  -> Out.LoopDeclarations input output st monoid
  -> In.LoopBody input st
  -> IdentifierMap Type
  -> IdentifierMap Type
addLoopBodyContext inputSz stateSz Out.LoopDeclarations{inputArrays,initialStates} In.LoopBody{indexId,inputArrays=inputElems,state=inputStates} ctx0 =
  let ctx1 = Identifier.insert indexId Type.Int ctx0
      ctx2 = Boxed.foldrZipWith
        (\In.NamedIdentifier{id} Out.OrderedExpr{expr=Out.AnnoTerm{typ}} ctx -> case typ of
          Type.Array ty -> Identifier.insert id ty ctx
          _ -> errorWithoutStackTrace "addLoopBodyContext: implementation mistake"
        )
        ctx1
        inputSz
        inputElems
        inputArrays
      ctx3 = Boxed.foldrZipWith
        (\In.NamedIdentifier{id} Out.AnnoTerm{typ} ctx ->
          Identifier.insert id typ ctx
        )
        ctx2
        stateSz
        inputStates
        initialStates
   in ctx3

-- TODO: bring monoidal results into scope
addLoopEpilogueContext ::
     Arithmetic.Nat output
  -> Arithmetic.Nat st
  -> Out.LoopDeclarations input output st monoid
  -> In.LoopEpilogue st output monoid
  -> IdentifierMap Type
  -> IdentifierMap Type
addLoopEpilogueContext inputSz stateSz Out.LoopDeclarations{outputArrays=outputTys,initialStates} In.LoopEpilogue{outputArrays,state=inputStates} ctx0 =
  let ctx1 = Boxed.foldrZipWith
        (\In.NamedIdentifier{id} Type.LoopArray{typ} ctx ->
          Identifier.insert id (Type.Array typ) ctx
        )
        ctx0
        inputSz
        outputArrays
        outputTys
      ctx2 = Boxed.foldrZipWith
        (\In.NamedIdentifier{id} Out.AnnoTerm{typ} ctx ->
          Identifier.insert id typ ctx
        )
        ctx1
        stateSz
        inputStates
        initialStates
   in ctx2

typesynthLoopDeclarations ::
     ( ?funcs :: FunctionIdentifierMap FunctionDescription
     , ?dcons :: ConIdentifierMap ConDescription
     )
  => IdentifierMap Type
  -> In.LoopDeclarations input output st monoid
  -> M (Out.LoopDeclarations input output st monoid)
typesynthLoopDeclarations ctx In.LoopDeclarations{length,inputArrays,initialStates,outputArrays} = do
  length'@Out.AnnoTerm{typ=lengthTy} <- typesynthAnnoTerm ctx LoopIdentifier.empty length
  case lengthTy of
    Type.Int -> pure ()
    _ -> failure "Expected length to be of type Int"
  inputArrays' <- traverse
    (\In.OrderedExpr{order,expr} -> do
      expr'@Out.AnnoTerm{typ=ty} <- typesynthAnnoTerm ctx LoopIdentifier.empty expr
      case ty of
        Type.Array{} -> pure ()
        _ -> failure "Expected for-loop arrays to have type Array"
      pure Out.OrderedExpr{order,expr=expr'}
    ) inputArrays
  initialStates' <- traverse (typesynthAnnoTerm ctx LoopIdentifier.empty) initialStates
  pure Out.LoopDeclarations
    { length = length'
    , inputArrays = inputArrays'
    , initialStates = initialStates'
    , outputArrays
    , monoids = ()
    }

typesynthAnnoTerm ::
     ( ?funcs :: FunctionIdentifierMap FunctionDescription
     , ?dcons :: ConIdentifierMap ConDescription
     )
  => IdentifierMap Type
  -> LoopIdentifierMap ContinueExpectation
  -> In.AnnoTerm
  -> M Out.AnnoTerm
typesynthAnnoTerm ctx lctx In.AnnoTerm{name,anno,term=term0} = case term0 of
  In.For id loopName inputSz outputSz stateSz decls body epilogue -> do
    let In.LoopBody{expr=bodyExpr} = body
    let In.LoopEpilogue{expr=epilogueExpr} = epilogue
    decls' <- typesynthLoopDeclarations ctx decls
    bodyExpr'@Out.AnnoTerm{typ=bodyType} <- typesynthAnnoTerm
      (addLoopBodyContext inputSz stateSz decls' body ctx)
      (LoopIdentifier.insert id (makeContinueExpectation decls') lctx)
      bodyExpr
    case bodyType of
      Type.Top -> pure ()
      _ -> failure "Expected loop body to call continue"
    epilogueExpr'@Out.AnnoTerm{typ=epilogueType} <- typesynthAnnoTerm
      (addLoopEpilogueContext outputSz stateSz decls' epilogue ctx)
      lctx
      epilogueExpr
    let body' = Out.LoopBody
          { indexName = let In.LoopBody{indexName=x} = body in x
          , indexId = let In.LoopBody{indexId=x} = body in x
          , inputArrays = let In.LoopBody{inputArrays=x} = body in x
          , state = let In.LoopBody{state=x} = body in x
          , expr = bodyExpr'
          }
    let epilogue' = Out.LoopEpilogue
          { monoids = ()
          , outputArrays = let In.LoopEpilogue{outputArrays=x} = epilogue in x
          , state = let In.LoopEpilogue{state=x} = epilogue in x
          , expr = epilogueExpr'
          }
    pure Out.AnnoTerm
      { typ = epilogueType
      , anno
      , name
      , term = Out.For id loopName inputSz outputSz stateSz decls' body' epilogue'
      }
  In.Continue id loopName states0 elems0 monoids0 -> case LoopIdentifier.lookup id lctx of
    Nothing -> failure "Missing for loop"
    Just ContinueExpectation{states,elems,monoids} -> do
      states' <- traverse (typesynthAnnoTerm ctx LoopIdentifier.empty) states0
      elems' <- traverse (typesynthAnnoTerm ctx LoopIdentifier.empty) elems0
      monoids' <- traverse (typesynthAnnoTerm ctx LoopIdentifier.empty) monoids0
      when (fmap getTermType states' /= states) (failure "one of the states had the wrong type")
      let expectedElemsLen = PM.sizeofArray elems
      let actualElemsLen = PM.sizeofArray elems'
      when (expectedElemsLen /= actualElemsLen) $ do
        failure ("wrong number of elements passed to continue in loop body, expected " ++ show expectedElemsLen ++ " but got " ++ show actualElemsLen)
      when (fmap getTermType elems' /= elems) (failure "one of the elements had the wrong type")
      when (fmap getTermType monoids' /= monoids) (failure "one of the monoids had the wrong type")
      let term = Out.Continue id loopName states' elems' monoids'
      pure Out.AnnoTerm{typ=Type.Top,anno,name,term}
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
    expr'@Out.AnnoTerm{typ=exprAnno} <- typesynthAnnoTerm ctx' lctx expr
    let term = Out.LetMany bnds' expr'
    pure (Out.AnnoTerm{typ=exprAnno,anno,name,term})
  In.Apply funcId (Identity tyArgs) args0 -> do
    args <- traverse (typesynthAnnoTerm ctx LoopIdentifier.empty) args0
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
    scrutinee'@Out.AnnoTerm{typ=scrutTy} <- typesynthAnnoTerm ctx lctx scrutinee
    case scrutTy of
      Type.Bool -> pure ()
      _ -> failure "Cannot perform case-bool on non boolean type"
    onTrue'@Out.AnnoTerm{typ=onTrueTy} <- typesynthAnnoTerm ctx lctx onTrue
    onFalse'@Out.AnnoTerm{typ=onFalseTy} <- typesynthAnnoTerm ctx lctx onFalse
    when (onTrueTy /= onFalseTy)
      (failure "Arms of case-bool did not have same type")
    let term = Out.CaseBool scrutinee' onTrue' onFalse'
    pure Out.AnnoTerm{typ=onTrueTy,anno,name,term}
  -- TODO: rewrite ApplyPrimitive with a more sustainable strategy that
  -- does not require modifying it every time a primop is added.
  In.ApplyPrimitive typeArity termArity op (Identity typeArgs) args -> do
    args' <- traverse (typesynthAnnoTerm ctx LoopIdentifier.empty) args
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
      Prim.ArrayReplicate
        | elemTy <- Boxed.index Lt.constant typeArgs Nat.zero
        , Out.AnnoTerm{typ=lenTy} <- Boxed.index Lt.constant args' Nat.zero
        , Out.AnnoTerm{typ=elTy} <- Boxed.index Lt.constant args' Nat.one
          -> do
            case lenTy of
              Type.Int -> pure ()
              _ -> failure "Expected first argument of arrayReplicate to be Int"
            if elTy == elemTy
              then pure ()
              else failure "Second argument to arrayIndex was of wrong type"
            pure (Type.Array elemTy)
    let term = Out.ApplyPrimitive typeArity termArity op (Identity typeArgs) args'
    pure Out.AnnoTerm{typ=outTy,anno,name,term}
  In.Project expr field -> do
    expr'@Out.AnnoTerm{typ=exprTy} <- typesynthAnnoTerm ctx lctx expr
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
  expr' <- typesynthAnnoTerm ctx LoopIdentifier.empty expr
  pure Out.Binding{id,name,expr=expr'}

substituteType ::
     Arithmetic.Nat n
  -> Boxed.Vector n TyVarIdentifier -- variable names
  -> Boxed.Vector n Type -- substitution of each variable
  -> Type -- type in which substitution is performed
  -> M Type
substituteType !n vars args ty0 = case ty0 of
  Type.Top -> pure Type.Top
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
    xs' <- traverse (typesynthAnnoTerm ctx LoopIdentifier.empty) xs
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
  expr' <- typesynthAnnoTerm ctx LoopIdentifier.empty expr
  pure Out.ConstructionField{name,expr=expr'}
