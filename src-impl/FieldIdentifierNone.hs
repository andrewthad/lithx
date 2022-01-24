module FieldIdentifierNone
  ( FieldIdentifier
  , FieldCollection
  , traverseFieldCollection
  ) where

import Data.Primitive (SmallArray)

import qualified Data.Traversable as Traversable

type FieldIdentifier = ()
type FieldCollection = SmallArray

traverseFieldCollection :: Monad m => (a -> m b) -> SmallArray a -> m (SmallArray b)
traverseFieldCollection = Traversable.traverse

