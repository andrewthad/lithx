module FieldIdentifierInt
  ( FieldIdentifier
  , FieldCollection
  , insertFieldMap
  , emptyFieldMap
  , traverseFieldCollection
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.Traversable as Traversable
import qualified Data.Foldable as Foldable
import qualified Data.IntMap.Strict as IntMap

type FieldIdentifier = Int
type FieldCollection = IntMap

-- foldFieldMapM :: Monad m => (b -> FieldIdentifier -> a -> m b) -> b -> FieldMap a -> m b
-- foldFieldMapM = error "write this function"

traverseFieldCollection :: Monad m => (a -> m b) -> IntMap a -> m (IntMap b)
traverseFieldCollection = Traversable.traverse

insertFieldMap :: FieldIdentifier -> a -> IntMap a -> IntMap a
insertFieldMap = IntMap.insert

emptyFieldMap :: IntMap a
emptyFieldMap = IntMap.empty
