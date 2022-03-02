{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}

module TermAnnotationShapeShow
  ( showAsSuffix
  ) where

import Prelude hiding (id)

import Shape (Shape(..),Metadata(..),Alias(..))
import Data.Text.Short (ShortText)

import qualified Data.Text.Short as TS
import qualified Identifier

showAsSuffix :: Metadata -> ShortText
showAsSuffix Metadata{alias,shape} =
  (case alias of
    Nothing -> TS.empty
    Just Alias{id,name} -> TS.pack "+" <> name <> Identifier.showAsSuffix id
  ) <> showShapeAsSuffix shape

showShapeAsSuffix :: Shape -> ShortText
showShapeAsSuffix = \case
  ShapeUnknown -> TS.empty
  ShapeInt i -> TS.pack ("+[" ++ show i ++ "]")
  ShapeArray m -> TS.pack "+array" -- TODO: show more in here
