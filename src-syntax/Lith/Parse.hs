{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}

module Lith.Parse
  ( parseFunction
  ) where

import Prelude hiding (id)
import Data.Foldable (foldl',toList)
import Data.Primitive (SmallArray,Array)
import Data.Text.Short (ShortText)
import Data.List.NonEmpty2 (NonEmpty2((:||)))
import Data.List.NonEmpty2 (NonEmpty((:|)))
import Data.Eexpr.Types (Eexpr)
import Lith.Primitive (Primitive)
import Arithmetic.Nat ((=?))
import Data.Functor.Identity (Identity(Identity))
import Order (Order(Asc,Desc))


import qualified Data.Primitive.Contiguous as C
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts

import qualified Arithmetic.Types as Arithmetic
import qualified Arithmetic.Nat as Nat
import qualified Data.Eexpr.Text as E
import qualified Data.Eexpr.Types as E
import qualified Data.Text.Short as TS
import qualified Lith.Syntax.Out as Out
import qualified Lith.Primitive as Prim
import qualified Vector.Boxed as Boxed

-- Examples:
--
-- define lookupDef (Trie Person, Bits, Person) -> Person
-- as (trieInit,key,def):
parseFunction :: [Eexpr ann] -> Either String (Out.FunctionDeclaration, [Eexpr ann])
parseFunction xs = case xs of
  [] -> Left "Need two lines for functions declaration, got zero"
  _ : [] -> Left "Need two lines for function declaration, got one"
  def : body : ys -> case def of
    E.Space _ (E.Symbol _ "define" :|| (E.Symbol _ name :| [ argTysExpr, E.Symbol _ "->", resTyExpr])) -> do 
      argTypes <- parseTypeTuple argTysExpr
      resType <- parseOneType resTyExpr
      case body of
        E.Space _ (E.Symbol _ "as" :|| (argNamesExpr :| [E.Block _ bodyExprs])) -> do
          argNames <- parseIdentifierTuple argNamesExpr
          Boxed.with argTypes $ \argTypes' -> Boxed.with argNames $ \argNames' -> do
            let argTypesLen = Boxed.length argTypes'
            let argNamesLen = Boxed.length argNames'
            eq <- case argNamesLen =? argTypesLen of
              Nothing -> Left "Wrong number of type arguments for function application"
              Just eq -> pure eq
            let bodyExpr0 :| bodyExprOther = bodyExprs
            parsedBody <- parseExpr bodyExpr0 bodyExprOther
            let decl = Out.FunctionDeclaration
                  { name=name
                  , id=()
                  , argLength=argTypesLen
                  , argTypes=argTypes'
                  , argNames=Boxed.substitute eq argNames'
                  , argIds=Boxed.replicate argTypesLen ()
                  , resType
                  , definition=nullAnno parsedBody
                  }
            pure (decl,ys)
        _ -> Left "invalid second line of function declaration"
    _ -> Left "invalid function declaration"

parseTypeTuple :: E.Eexpr ann -> Either String (Array Out.Type)
parseTypeTuple = \case
  E.Paren _ (Just (E.Comma _ symbols)) -> fmap Exts.fromList (mapM parseOneType symbols)
  e -> fmap C.singleton (parseOneType e)

parseOneType :: E.Eexpr ann -> Either String Out.Type
parseOneType = \case
  E.Symbol _ s -> case s of
    "Int" -> pure Out.Int
    "Bits" -> pure Out.Bits
    "Bool" -> pure Out.Bool
    _ -> Left ("unrecognized type: " ++ TS.unpack s)
  E.Space _ (E.Symbol _ "Array" :|| t :| []) -> fmap Out.Array (parseOneType t)
  E.Space _ (E.Symbol _ con :|| t :| ts) -> do
    ts' <- traverse parseOneType (t : ts)
    pure (Out.Box () con (Exts.fromList ts'))
  E.Paren _ (Just (E.Space _ (E.Symbol _ "Array" :|| t :| []))) -> fmap Out.Array (parseOneType t)
  e -> Left ("unrecognized type: " ++ show (E.mapAnnotation (\_ -> ()) e))

parseIdentifierTuple :: E.Eexpr ann -> Either String (Array ShortText)
parseIdentifierTuple = \case
  E.Paren _ (Just (E.Comma _ symbols)) -> fmap Exts.fromList (mapM parseOneIdentifier symbols)
  E.Paren _ (Just symbol) -> fmap C.singleton (parseOneIdentifier symbol)
  _ -> Left "expected parenthesized expression"

parseOneIdentifier :: E.Eexpr ann -> Either String ShortText
parseOneIdentifier = \case
  E.Symbol _ s -> Right s
  _ -> Left "expected symbol when parsing identifier"

parseLoopArray :: Eexpr ann -> Either String Out.LoopArray
parseLoopArray = \case
  E.Space _ (E.Symbol _ theOrder :|| ty :| []) -> do
    order <- case theOrder of
      "asc" -> Right Asc
      "desc" -> Right Desc
      _ -> Left "expected output array order to be asc or desc"
    typ <- parseOneType ty
    pure Out.LoopArray
      { order
      , typ
      }
  _ -> Left "could not output array declaration"

parseOrderedExpr :: Eexpr ann -> Either String Out.OrderedExpr
parseOrderedExpr = \case
  E.Space _ (E.Symbol _ theOrder :|| expr0 :| []) -> do
    order <- case theOrder of
      "asc" -> Right Asc
      "desc" -> Right Desc
      _ -> Left "expected output array order to be asc or desc"
    expr <- parseAtomic expr0
    pure Out.OrderedExpr
      { order
      , expr=nullAnno expr
      }
  _ -> Left "could not output array declaration"

-- parseStateBinding :: Eexpr ann -> Either String Out.Binding
-- parseStateBinding = \case
--   E.Space _ (E.Symbol _ name :|| E.Symbol _ "=" :| [expr]) -> do
--     expr' <- parseAtomic expr
--     pure Out.Binding
--       { id=()
--       , name
--       , expr=nullAnno expr'
--       }
--   _ -> Left "could not parse state binding"

parseExpr :: E.Eexpr ann -> [E.Eexpr ann] -> Either String Out.Term
parseExpr e es = case es of
  [] -> case e of
    E.Space _ (E.Symbol _ "case" :|| (E.Brace _ (Just (E.Symbol _ "Bool")) :| [E.Symbol _ "on", scrutinee,E.Symbol _ "of",arms])) -> do
      parsedScrutinee <- parseAtomic scrutinee
      (onTrue,onFalse) <- case arms of
        E.Block _ (onTrue :| [onFalse]) -> pure (onTrue,onFalse)
        _ -> Left "Expected exactly two arms for case-bool"
      parsedOnTrue <- parseArmWithPattern "True" onTrue
      parsedOnFalse <- parseArmWithPattern "False" onFalse
      let annoScrutinee = nullAnno parsedScrutinee
      pure (Out.CaseBool annoScrutinee (nullAnno parsedOnTrue) (nullAnno parsedOnFalse))
    E.Space _ (E.Chain _ ((E.Symbol _ "for") :|| E.Paren _ range :| [E.Paren _ inputArraysExpr, E.Paren _ statesExpr,E.Paren _ _,E.Paren _ outputArraysExpr]) :|| E.Block _ sections :| []) -> do
      len <- case range of
        Just range' -> parseAtomic range'
        Nothing -> Left "for loop was missing length"
      -- (start,len) <- case range of
      --   Just (E.Comma _ [start,len]) -> do
      --     start' <- parseAtomic start
      --     len' <- parseAtomic len
      --     pure (start',len')
      --   _ -> Left "for loop had malformed range, expected two elements"
      inputArrays0 <- case inputArraysExpr of
        Nothing -> pure mempty
        Just (E.Comma _ bnds) -> fmap Exts.fromList (traverse parseOrderedExpr bnds)
        Just other -> fmap C.singleton (parseOrderedExpr other)
      initialStates0 <- case statesExpr of
        Nothing -> pure mempty
        Just (E.Comma _ bnds) -> fmap Exts.fromList (traverse parseAtomic bnds)
        Just other -> fmap C.singleton (parseAtomic other)
      outputArrays0 <- case outputArraysExpr of
        Nothing -> pure mempty
        Just (E.Comma _ descrs) -> fmap Exts.fromList (traverse parseLoopArray descrs)
        Just other -> fmap C.singleton (parseLoopArray other)
      Boxed.with inputArrays0 $ \inputArrays -> Boxed.with initialStates0 $ \initialStates -> Boxed.with outputArrays0 $ \outputArrays -> do
        let inputSize = Boxed.length inputArrays
        let stateSize = Boxed.length initialStates
        let outputSize = Boxed.length outputArrays
        (body',epilogue') <- case sections of
          body :| [epilogue] -> do
            body' <- parseLoopBody inputSize stateSize body
            epilogue' <- parseLoopEpilogue stateSize outputSize epilogue
            pure (body',epilogue')
          _ -> Left "expected exactly two sections in for loop"
        let decls = Out.LoopDeclarations
              { length = nullAnno len
              , inputArrays
              , initialStates=fmap nullAnno initialStates
              , outputArrays
              , monoids = ()
              }
        pure $ Out.For
          ()
          "_"
          inputSize
          outputSize
          stateSize
          decls
          body'
          epilogue'
    _ -> parseAtomic e
  x : xs -> case e of
    (E.Space _ (E.Symbol _ var :|| (E.Symbol _ "=" :| rhs))) -> do
      parsedRhs <- case rhs of
        [rhsOne] -> parseAtomic rhsOne
        _ -> Left "could not parse RHS of let binding"
      expr <- parseExpr x xs
      let annoExpr = nullAnno expr
      pure (Out.LetMany (C.singleton (Out.Binding{id=(),name=var,expr=nullAnno parsedRhs})) annoExpr)
    _ -> Left "expected assignment on nonterminal expression line"

parseLoopBody ::
     Arithmetic.Nat input
  -> Arithmetic.Nat state
  -> Eexpr ann
  -> Either String (Out.LoopBody input state)
parseLoopBody !inputSize !stateSize ex = case ex of
  E.Space _ (E.Chain _ (E.Symbol _ "body" :|| E.Paren _ (Just (E.Symbol _ ixName)) :| [E.Paren _ inputArrays0, E.Paren _ stateArrays0]) :|| E.Symbol _ "=>" :| [rhs]) -> do
    rhs' <- case rhs of
      E.Block _ (e0 :| es) -> parseExpr e0 es
      _ -> parseAtomic rhs
    inputArrays <- case inputArrays0 of
      Just (E.Comma _ inputArrays1) -> do
        inputArrays2 <- traverse
          (\e -> case e of
            E.Symbol _ name -> pure Out.NamedIdentifier{name,id=()}
            _ -> Left "expected loop body element idents to all be symbols"
          ) (toList inputArrays1)
        case Boxed.fromList inputSize inputArrays2 of
          Nothing -> Left "wrong number of input element identifiers in loop body"
          Just res -> pure res
      Just (E.Symbol _ soleName) -> case Nat.one =? inputSize of
        Just eq -> do
          let res = Out.NamedIdentifier{name=soleName,id=()}
          pure (Boxed.substitute eq (Boxed.singleton res))
        Nothing -> Left "loop body had different number of element idents than decl"
      _ -> Left "expected loop body element idents to be either comma-separated or single symbol"
    state <- case stateArrays0 of
      Just (E.Comma _ stateArrays1) -> do
        stateArrays2 <- traverse
          (\e -> case e of
            E.Symbol _ name -> pure Out.NamedIdentifier{name,id=()}
            _ -> Left "expected loop body state idents to all be symbols"
          ) (toList stateArrays1)
        case Boxed.fromList stateSize stateArrays2 of
          Nothing -> Left "wrong number of state identifiers in loop body"
          Just res -> pure res
      Just (E.Symbol _ soleName) -> case Nat.one =? stateSize of
        Just eq -> do
          let res = Out.NamedIdentifier{name=soleName,id=()}
          pure (Boxed.substitute eq (Boxed.singleton res))
        Nothing -> Left "loop body had different number of state idents than decl"
      _ -> Left "expected loop body element idents to be either comma-separated or single symbol"
    pure Out.LoopBody
      { indexName = ixName
      , indexId = ()
      , inputArrays
      , state
      , expr = nullAnno rhs'
      }
  _ -> Left ("malformed loop body: " ++ show (E.mapAnnotation (\_ -> ()) ex))

parseLoopEpilogue ::
     Arithmetic.Nat state
  -> Arithmetic.Nat output
  -> Eexpr ann
  -> Either String (Out.LoopEpilogue state output monoid)
parseLoopEpilogue !stateSize !outputSize ex = case ex of
  E.Space _ (E.Chain _ (E.Symbol _ "epilogue" :|| E.Paren _ stateArrays0 :| [E.Paren _ outputArrays0, E.Paren _ _]) :|| E.Symbol _ "=>" :| [rhs]) -> do
    -- todo: add monoids
    rhs' <- case rhs of
      E.Block _ (e0 :| es) -> parseExpr e0 es
      _ -> parseAtomic rhs
    outputArrays <- case outputArrays0 of
      Just (E.Comma _ outputArrays1) -> do
        outputArrays2 <- traverse
          (\e -> case e of
            E.Symbol _ name -> pure Out.NamedIdentifier{name,id=()}
            _ -> Left "expected loop epilogue element idents to all be symbols"
          ) (toList outputArrays1)
        case Boxed.fromList outputSize outputArrays2 of
          Nothing -> Left "wrong number of output element identifiers in loop epilogue"
          Just res -> pure res
      Just (E.Symbol _ soleName) -> case Nat.one =? outputSize of
        Just eq -> do
          let res = Out.NamedIdentifier{name=soleName,id=()}
          pure (Boxed.substitute eq (Boxed.singleton res))
        Nothing -> Left "loop epilogue had different number of element idents than decl"
      Nothing -> case Nat.zero =? outputSize of
        Just eq -> pure (Boxed.substitute eq Boxed.empty)
        Nothing -> Left "loop epilogue had zero output array idents, but decl had did not"
      _ -> Left "expected loop epilogue element idents to be either comma-separated or single symbol"
    state <- case stateArrays0 of
      Just (E.Comma _ stateArrays1) -> do
        stateArrays2 <- traverse
          (\e -> case e of
            E.Symbol _ name -> pure Out.NamedIdentifier{name,id=()}
            _ -> Left "expected loop epilogue state idents to all be symbols"
          ) (toList stateArrays1)
        case Boxed.fromList stateSize stateArrays2 of
          Nothing -> Left "wrong number of state identifiers in loop epilogue"
          Just res -> pure res
      Just (E.Symbol _ soleName) -> case Nat.one =? stateSize of
        Just eq -> do
          let res = Out.NamedIdentifier{name=soleName,id=()}
          pure (Boxed.substitute eq (Boxed.singleton res))
        Nothing -> Left "loop epilogue had different number of state idents than decl"
      _ -> Left "expected loop epilogue element idents to be either comma-separated or single symbol"
    pure Out.LoopEpilogue
      { outputArrays
      , state
      , monoids=()
      , expr = nullAnno rhs'
      }
  _ -> Left ("malformed loop epilogue: " ++ show (E.mapAnnotation (\_ -> ()) ex))

parseArmWithPattern :: ShortText -> Eexpr ann -> Either String Out.Term
parseArmWithPattern expected e = case e of
  E.Space _ (E.Symbol _ pat :|| E.Symbol _ "=>" :| [rhs]) ->
    if pat == expected
      then case rhs of
        E.Block _ (e0 :| es) -> parseExpr e0 es
        _ -> parseAtomic rhs
      else Left "expected case arm to have different pattern"
  _ -> Left ("malformed case arm: " ++ show (E.mapAnnotation (\_ -> ()) e))
    

nullAnno :: Out.Term -> Out.AnnoTerm
nullAnno t = Out.AnnoTerm
  { typ = ()
  , anno = ()
  , name = ""
  , term = t
  }

parseAtomic :: E.Eexpr ann -> Either String Out.Term
parseAtomic = \case
  E.Number _ b
    | E.Bignum {significand=sf, radix = E.Radix 10, fractionalExponent = 0, explicitExponent = 0} <- b
        -> pure (Out.Literal (Out.LitInt (fromInteger sf)))
    | E.Bignum {significand=sf, radix = E.Radix 2, fractionalExponent = 0, explicitExponent = 0} <- b
        -> pure (Out.Literal (Out.LitBits (fromInteger sf)))
    | otherwise -> Left "sorry, only a few numbers are supported"
  E.Symbol _ s -> case s of
    "True" -> Right (Out.Literal Out.LitTrue)
    "False" -> Right (Out.Literal Out.LitFalse)
    _ -> Right (Out.Var () s)
  -- E.Chain _ (E.Symbol _ objName :|| (E.Symbol _ fieldName :| others)) -> case others of
  --   [] -> do
  --     fieldId <- parseFieldName fieldName
  --     Right (E0.Project () (E0.Var () (TS.toText objName)) fieldId)
  --   [E.Bracket _ (Just ix)] -> do
  --     fieldId <- parseFieldName fieldName
  --     ix' <- parseAtomic ix
  --     Right (E0.IndexUnknown (E0.Project () (E0.Var () (TS.toText objName)) fieldId) ix')
  --   _ -> Left "figure out how to support nested chains better"
  E.Chain _ (E.Symbol _ funcName :|| (E.Paren _ args) :| []) -> do
    args' <- parseArgs args
    -- TODO: Specifically check if it starts with dollar
    case funcName of
      "$add" -> finishPrimArgs Prim.Add Nat.constant Nat.constant args'
      "$multiply" -> finishPrimArgs Prim.Multiply Nat.constant Nat.constant args'
      "$negate" -> finishPrimArgs Prim.Negate Nat.constant Nat.constant args'
      _ -> Left "Sorry, not many functions can be parsed"
  E.Chain _ (E.Symbol _ "continue" :|| (E.Paren _ statesExpr) :| [E.Paren _ outputArraysExpr, E.Paren _ outputMonoidsExpr]) -> do
    statesExpr' <- parseArgs statesExpr
    outputArraysExpr' <- parseArgs outputArraysExpr
    outputMonoidsExpr' <- parseArgs outputMonoidsExpr
    pure (Out.Continue () "_" statesExpr' outputArraysExpr' outputMonoidsExpr')
  E.Chain _ (E.Symbol _ funcName :|| E.Brace _ (Just tyArg) :| [E.Paren _ args]) -> do
    args' <- parseArgs args
    ty <- parseOneType tyArg
    -- TODO: Specifically check if it starts with dollar
    case funcName of
      "$arrayIndex" -> finishPrimArgs1 Prim.ArrayIndex Nat.constant Nat.constant ty args'
      "$arrayReplicate" -> finishPrimArgs1 Prim.ArrayReplicate Nat.constant Nat.constant ty args'
      _ -> Left "Sorry, not many functions can be parsed"
      -- _ -> do
      --   args' <- parseArgs args
      --   pure (E0.Apply () (TS.toText funcName) Map.empty args')
  -- E.Chain _ (E.Symbol _ arrName :|| (E.Bracket _ (Just (E.Comma _ [start,len]))) :| []) -> do
  --   start' <- parseAtomic start
  --   len' <- parseAtomic len
  --   pure (E0.SliceUnknown (E0.Var () (TS.toText arrName)) start' len')
  -- E.Chain _ (E.Symbol _ arrName :|| (E.Bracket _ (Just ix)) :| []) -> do
  --   ix' <- parseAtomic ix
  --   pure (E0.IndexUnknown (E0.Var () (TS.toText arrName)) ix')
  -- E.Paren _ inside -> case inside of
  --   Nothing -> Left "Empty parens not allowed"
  --   Just e -> parseAtomic e
  -- E.Bracket _ inside -> case inside of
  --   Nothing -> Left "Empty array literal not allowed"
  --   Just (E.Comma _ es) -> do
  --     es' <- traverse parseAtomic (Exts.toList es)
  --     pure (E0.LitArray () (Exts.fromList es'))
  --   Just e -> do
  --     e' <- parseAtomic e
  --     pure (E0.LitArray () (Exts.fromList [e']))
  -- e@(E.Space _ inner) -> case inner of
  --   a :|| E.Symbol _ "+" :| [b] -> do
  --     a' <- parseAtomic a
  --     b' <- parseAtomic b
  --     Right (E0.Add a' b')
  --   a :|| E.Symbol _ "<" :| [b] -> do
  --     a' <- parseAtomic a
  --     b' <- parseAtomic b
  --     Right (E0.LessThan a' b')
  --   _ -> Left ("add more atomic things that come from space-separated expressions: " ++ show (E.mapAnnotation (\_ -> ()) e))
  e -> error ("parseAtomic: need to handle this: " ++ show (E.mapAnnotation (\_ -> ()) e))

parseArgs :: Maybe (E.Eexpr ann) -> Either String (Array Out.AnnoTerm)
parseArgs = \case
  Nothing -> Right mempty
  Just (E.Comma _ xs) -> fmap Exts.fromList (mapM (fmap nullAnno . parseAtomic) xs)
  Just v -> fmap (C.singleton . nullAnno) (parseAtomic v)

finishPrimArgs ::
     Primitive 0 terms
  -> Arithmetic.Nat 0
  -> Arithmetic.Nat terms
  -> Array Out.AnnoTerm
  -> Either String Out.Term
finishPrimArgs op !types !terms args = Boxed.with args $ \args' -> do
  let argsLen = Boxed.length args'
  eqArgs <- case argsLen =? terms of
    Nothing -> Left "Wrong number of arguments to primop"
    Just eq -> pure eq
  pure $ Out.ApplyPrimitive types terms op (Identity Boxed.empty) (Boxed.substitute eqArgs args')

finishPrimArgs1 ::
     Primitive 1 terms
  -> Arithmetic.Nat 1
  -> Arithmetic.Nat terms
  -> Out.Type
  -> Array Out.AnnoTerm
  -> Either String Out.Term
finishPrimArgs1 op !types !terms ty args = Boxed.with args $ \args' -> do
  let argsLen = Boxed.length args'
  eqArgs <- case argsLen =? terms of
    Nothing -> Left "Wrong number of arguments to primop"
    Just eq -> pure eq
  pure $ Out.ApplyPrimitive types terms op (Identity (Boxed.singleton ty)) (Boxed.substitute eqArgs args')
