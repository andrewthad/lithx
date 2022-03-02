{-# language KindSignatures #-}

module ConIdentifierNone
  ( ConIdentifier
  ) where

import Prelude hiding (lookup)

import Data.Proxy
import qualified Data.Kind as GHC

type ConIdentifier = ()
