{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language GADTs #-}

-- Reexport the data types to make it easier for to reference them.
module Lith.Show
  ( showFunction
  ) where

import Prelude hiding (id)
import Data.Maybe (fromMaybe)
import Data.Foldable (foldl',foldrM)
import Data.Primitive (SmallArray,Array)
import Data.Text.Short (ShortText)
import Data.List.NonEmpty2 (NonEmpty2((:||)))
import Data.List.NonEmpty2 (NonEmpty((:|)))
import Data.Eexpr.Types (Eexpr)
import Lith.Primitive (Primitive)
import Arithmetic.Nat ((=?))
import Identifier (Identifier)


import qualified Data.List.NonEmpty as NE
import qualified Data.Primitive.Contiguous as C
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts

import qualified Arithmetic.Types as Arithmetic
import qualified Arithmetic.Nat as Nat
import qualified Data.Eexpr.Text as E
import qualified Data.Eexpr.Types as E
import qualified Lith.Term as In
import qualified Lith.Primitive as Prim
import qualified Vector.Boxed as Boxed
import qualified Identifier
import qualified TermAnnotation
import qualified TermAnnotationShow

-- Examples:
--
-- define lookupDef (Trie Person, Bits, Person) -> Person
-- as (trieInit,key,def):
showFunction :: In.FunctionDeclaration -> (Eexpr (), Eexpr ())
showFunction In.FunctionDeclaration{name,argLength,argTypes,argNames,argIds,resType,definition} =
  ( E.Space () (E.Symbol () "define" :|| E.Symbol () name :| [showTypes (Boxed.forget argTypes), E.Symbol () "->", showType resType])
  , E.Space () (E.Symbol () "as" :|| showArgNames argLength argNames argIds :| [E.Block () (showAnnoTerm definition)])
  )

showAnnoTerm :: In.AnnoTerm -> NonEmpty (Eexpr ())
showAnnoTerm In.AnnoTerm{term} = showTerm term

showLoopDeclarations :: In.LoopDeclarations input output st monoid -> NonEmpty (E.Eexpr ())
showLoopDeclarations In.LoopDeclarations{length,inputArrays,initialStates,outputArrays}
  | Just length' <- showAtomic length =
      E.Paren () (Just length') :| []
  | otherwise = errorWithoutStackTrace "showLoopDeclarations: whoops"

showLoopBody ::
     Arithmetic.Nat input
  -> Arithmetic.Nat st
  -> In.LoopBody input st
  -> Eexpr ()
showLoopBody !szInput !szState In.LoopBody{indexName,indexId,inputArrays,state,expr} = E.Space ()
  (     E.Chain ()
        (     E.Symbol () "body"
          :|| E.Paren () (Just (showIdentifier indexName indexId))
          :|  [ E.Paren () (showCommaSepNamedIds szInput inputArrays)
              , E.Paren () (showCommaSepNamedIds szState state)
              ]
        )
    :|| E.Symbol () "=>"
     :| [E.Block () (showAnnoTerm expr)]
  )

showLoopEpilogue ::
     Arithmetic.Nat st
  -> Arithmetic.Nat output
  -> In.LoopEpilogue st output monoid
  -> Eexpr ()
showLoopEpilogue !szState !szOutput In.LoopEpilogue{state,outputArrays,expr} = E.Space ()
  (     E.Chain ()
        (     E.Symbol () "epilogue"
          :|| E.Paren () (showCommaSepNamedIds szState state)
          :|  [ E.Paren () (showCommaSepNamedIds szOutput outputArrays)
              , E.Paren () Nothing
              ]
        )
    :|| E.Symbol () "=>"
     :| [E.Block () (showAnnoTerm expr)]
  )

showIdentifier :: ShortText -> Identifier -> Eexpr ()
showIdentifier name id =
  E.Symbol () (name <> Identifier.showAsSuffix id)

showNamedIdentifier :: In.NamedIdentifier -> Eexpr ()
showNamedIdentifier In.NamedIdentifier{name,id} = showIdentifier name id

showCommaSepNamedIds ::
     Arithmetic.Nat n
  -> Boxed.Vector n In.NamedIdentifier
  -> Maybe (Eexpr ())
showCommaSepNamedIds !n v = case Nat.demote n of
  0 -> Nothing
  _ -> Just (E.Comma () (foldr (\ident acc -> showNamedIdentifier ident : acc) [] v))

showCommaSepAnnoTerms ::
     Array In.AnnoTerm
  -> Maybe (Eexpr ())
showCommaSepAnnoTerms v = case PM.sizeofArray v of
  0 -> Nothing
  _ -> Just (E.Comma () (foldr (\e acc -> fromMaybe (error "showCommaSepAnnoTerms: allow non-atomic") (showAtomic e) : acc) [] v))

showTerm :: In.Term -> NonEmpty (Eexpr ())
showTerm = \case
  In.Continue _ _ states outputArrays _ -> E.Chain ()
    (   E.Symbol () "continue"
    :|| E.Paren () (showCommaSepAnnoTerms states)
     :| [ E.Paren () (showCommaSepAnnoTerms outputArrays)
        , E.Paren () Nothing
        ]
    ) :| []
  In.For _ _ szInput szOutput szState decls body epilogue -> E.Space ()
    (   E.Chain () (E.Symbol () "for" :|| showLoopDeclarations decls)
    :|| E.Block ()
         (  showLoopBody szInput szState body
         :| showLoopEpilogue szState szOutput epilogue
          : []
         )
     :| []
    ) :| []
  In.Var ident name -> E.Symbol () (name <> Identifier.showAsSuffix ident) :| []
  In.Literal lit -> showLit lit :| []
  In.CaseBool scrut onTrue onFalse -> 
    let onTrueEnc = E.Space ()
          (E.Symbol () "True" :|| E.Symbol () "=>" :|
            case showAtomic onTrue of
              Just onTrue' -> [onTrue']
              Nothing -> [E.Block () (showAnnoTerm onTrue)]
          )
        onFalseEnc = E.Space ()
          (E.Symbol () "False" :|| E.Symbol () "=>" :|
            case showAtomic onFalse of
              Just onFalse' -> [onFalse']
              Nothing -> [E.Block () (showAnnoTerm onFalse)]
          )
     in case showAtomic scrut of
          Nothing -> error "showTerm: write this case for case-bool"
          Just scrut' -> E.Space ()
            (E.Symbol () "case"
            :|| E.Brace () (Just (E.Symbol () "Bool"))
            :| [ E.Symbol () "on"
               , scrut'
               , E.Symbol () "of"
               , E.Block () (onTrueEnc :| [onFalseEnc])
               ]
            ) :| []
  In.LetMany bnds e -> case PM.sizeofSmallArray bnds of
    0 -> showAnnoTerm e
    sz -> showBinding True (PM.indexSmallArray bnds 0)
      :| C.foldr (\bnd acc -> showBinding False bnd : acc) (NE.toList (showAnnoTerm e)) (C.slice bnds 1 (sz - 1))
  In.ApplyPrimitive _ termArity op _ terms -> case showArgs termArity terms of
    Just terms' ->
      -- Duplicated
      E.Chain () (E.Symbol () (encodePrimOp op) :|| (E.Paren () terms') :| []) :| []
    _ -> error "showTerm: ApplyPrimitive, write this case"

showBinding :: Bool -> In.Binding -> Eexpr ()
showBinding isLeader In.Binding{name,id,expr=expr@In.AnnoTerm{anno}} = case showAtomic expr of
  Just expr' -> 
    E.Space () (E.Symbol () (name <> Identifier.showAsSuffix id <> TermAnnotationShow.showAsSuffix anno) :|| (E.Symbol () (if isLeader then "=" else "=^")) :| [expr'])
  Nothing -> error "showBinding: write this"

-- The Maybe indicates a possibility of failure.
showAtomic :: In.AnnoTerm -> Maybe (Eexpr ())
showAtomic In.AnnoTerm{term} = case term of
  In.ApplyPrimitive _ termArity op _ terms -> do
    terms' <- showArgs termArity terms
    Just (E.Chain () (E.Symbol () (encodePrimOp op) :|| (E.Paren () terms') :| []))
  In.Var ident name -> Just (E.Symbol () (name <> Identifier.showAsSuffix ident))
  In.Literal lit -> Just (showLit lit)
  In.Project{} -> error "showAtomic: project"
  In.Apply{} -> error "showAtomic: apply"
  In.CaseBool{} -> Nothing
  In.LetMany{} -> Nothing
  _ -> error "showAtomic: write this case"

showArgs :: Arithmetic.Nat n -> Boxed.Vector n In.AnnoTerm -> Maybe (Maybe (Eexpr ()))
showArgs n args = case Nat.demote n of
  0 -> Just Nothing
  1 -> Just <$> showAtomic (PM.indexArray (Boxed.forget args) 0)
  _ -> do
    args' <- foldrM (\arg acc -> fmap (:acc) (showAtomic arg)) [] (Boxed.forget args)
    Just (Just (E.Comma () args'))

encodePrimOp :: Primitive typeArity termArity -> ShortText
encodePrimOp = \case
  Prim.Add -> "$add"
  Prim.Multiply -> "$multiply" 
  Prim.Negate -> "$negate" 
  Prim.ArrayIndex -> "$arrayIndex" 
  Prim.ArrayReplicate -> "$arrayReplicate" 

showLit :: In.Lit -> Eexpr ()
showLit = \case
  In.LitInt i -> E.Number () E.Bignum{significand=fromIntegral i,radix=E.Radix 10,fractionalExponent=0,explicitExponent=0}
  In.LitTrue -> E.Symbol () "True"
  In.LitFalse -> E.Symbol () "False"
  _ -> error "showLit: write this case"

showType :: In.Type -> E.Eexpr ()
showType = \case
  In.Int -> E.Symbol () "Int"
  In.Bool -> E.Symbol () "Bool"
  In.Bits -> E.Symbol () "Bits"
  In.Array x -> E.Space () (E.Symbol () "Array" :|| showType x :| [])
  _ -> error "showType: write this case"

showTypes :: Array In.Type -> E.Eexpr ()
showTypes ts = case PM.sizeofArray ts of
  0 -> E.Paren () Nothing
  1 -> showType (PM.indexArray ts 0)
  _ -> E.Paren () (Just (E.Comma () (foldr (\t acc -> showType t : acc) [] ts)))

showArgNames :: Arithmetic.Nat n -> Boxed.Vector n ShortText -> Boxed.Vector n Identifier -> E.Eexpr ()
showArgNames !n !names !idents = E.Paren ()
  (Just (E.Comma () (Boxed.foldrZipWith (\name ident acc -> E.Symbol () (name <> Identifier.showAsSuffix ident) : acc) [] n names idents)))
