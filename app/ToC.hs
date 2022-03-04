{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}

module ToC
  ( compileFunction
  ) where

import Data.Text.Short (ShortText)
import Data.Primitive (Array)
import Vector.Boxed (Vector)

import qualified Arithmetic.Types as Arithmetic
import qualified Lith.Datasolved as I
import qualified C
import qualified Data.Text.Short as TS
import qualified Vector.Boxed as Vector

makeName :: ShortText -> Int -> ShortText
makeName x y = x <> "_" <> TS.pack (show y)

compileFunction :: I.FunctionDeclaration -> C.Expr
compileFunction
  I.FunctionDeclaration{name,id,argLength,argTypes,argNames,argIds,resType,definition} =
  C.FunctionDeclaration
    (compileType resType)
    (makeName name 0)
    (compileFunctionArgs argLength argTypes argIds argNames)
    (compileAnnoTerm definition)

compileType :: I.Type -> C.Type
compileType = \case
  I.Int -> C.SignedInt64
  I.Array t -> C.Pointer (compileType t)

compileAnnoTerm :: I.AnnoTerm -> C.Expr
compileAnnoTerm _ = C.Return (C.Integer 1)

compileFunctionArgs ::
     Arithmetic.Nat n
  -> Vector n I.Type
  -> Vector n Int
  -> Vector n ShortText
  -> Array C.FunctionArgument
compileFunctionArgs n types ids names = Vector.forget $ Vector.zipWith3'
  (\typ id name -> C.FunctionArgument
    { name = makeName name id
    , typ = compileType typ
    }
  ) n types ids names

