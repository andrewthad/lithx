module TermAnnotationIntSet
  ( TermAnnotation
  , showAsSuffix
  ) where

import Data.Text.Short (ShortText)
import Data.IntSet (IntSet)

import qualified Data.Text.Short as TS
import qualified Data.List as List
import qualified Data.IntSet as IntSet

type TermAnnotation = IntSet

showAsSuffix :: IntSet -> ShortText
showAsSuffix s = TS.pack
  ('+' : '{' : (List.intercalate "," (map show (IntSet.toList s)) ++ "}"))
