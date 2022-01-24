module FieldIdentifierShortText
  ( FieldIdentifier
  , FieldMap
  , foldFieldMapM
  , insertFieldMap
  , emptyFieldMap
  ) where

import Data.Text.Short (ShortText)
import Data.Map.Strict (Map)

import qualified Data.Map.Strict as Map

type FieldIdentifier = ShortText
type FieldMap = Map ShortText

foldFieldMapM :: Monad m => (b -> FieldIdentifier -> a -> m b) -> b -> FieldMap a -> m b
foldFieldMapM = error "write this function"

insertFieldMap :: FieldIdentifier -> a -> FieldMap a -> FieldMap a
insertFieldMap = Map.insert

emptyFieldMap :: FieldMap a
emptyFieldMap = Map.empty
