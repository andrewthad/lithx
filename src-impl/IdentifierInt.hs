module IdentifierInt
  ( Identifier
  , IdentifierMap
  , IntMap.insert
  , IntMap.lookup
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

type Identifier = Int
type IdentifierMap = IntMap
