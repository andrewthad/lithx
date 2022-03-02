{-# language KindSignatures #-}

module FunctionIdentifierNone
  ( FunctionIdentifier
  , FunctionIdentifierMap
  , lookup
  ) where

import Prelude hiding (lookup)
import Data.Proxy
import qualified Data.Kind as GHC

type FunctionIdentifier = ()
type FunctionIdentifierMap = (Proxy :: GHC.Type -> GHC.Type)

-- Get rid of this
lookup :: FunctionIdentifier -> FunctionIdentifierMap a -> Maybe a
lookup _ _ = Nothing
