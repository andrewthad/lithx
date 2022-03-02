module FieldIdentifierNone
  ( FieldIdentifier
  , FieldCollection
  , zipM_
  , sameKeys
  , lookup
  ) where

import Prelude hiding (lookup)

import Data.Primitive (SmallArray)

import qualified Data.Traversable as Traversable

type FieldIdentifier = ()
type FieldCollection = SmallArray

-- This is bogus. Delete this.
zipM_ :: Monad m
  => (a -> b -> m c)
  -> FieldCollection a
  -> FieldCollection b
  -> m ()
zipM_ _ _ _ = pure ()

-- Delete this
sameKeys :: FieldCollection a -> FieldCollection b -> Bool
sameKeys _ _ = False

-- Delete this
lookup :: FieldIdentifier -> FieldCollection a -> Maybe a
lookup _ _ = Nothing
