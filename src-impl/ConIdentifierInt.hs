module ConIdentifierInt
  ( ConIdentifier
  , ConIdentifierMap
  , lookup
  ) where

import Prelude hiding (lookup)
import Data.IntMap.Strict (IntMap,lookup)

type ConIdentifier = Int
type ConIdentifierMap = IntMap
