module LoopIdentifierInt
  ( LoopIdentifier
  , LoopIdentifierMap
  , LoopIdentifierSet
  , IntMap.insert
  , IntMap.lookup
  , IntMap.empty
  , IntSet.union
  , IntSet.member
  , IntSet.singleton
  , showAsSuffix
  ) where

import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.Text.Short (ShortText)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Text.Short as TS

type LoopIdentifier = Int
type LoopIdentifierMap = IntMap
type LoopIdentifierSet = IntSet

showAsSuffix :: Int -> ShortText
showAsSuffix i = TS.pack ('@' : show i)

