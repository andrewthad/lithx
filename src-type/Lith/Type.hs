module Lith.Type
  ( Type(..)
  , LoopArray(..)
  ) where

import Order (Order)
import Data.Text.Short (ShortText)
import Data.Primitive (Array)
import ConIdentifier (ConIdentifier)
import TyVarIdentifier (TyVarIdentifier)

data Type
  = Top
    -- ^ The same as: forall a. a
  | Int
  | Bits
  | Bool
  | Array Type -- a slice of an array
  | Box
      !ConIdentifier -- type constructor
      !ShortText -- what was written in the source code
      !(Array Type) -- type arguments
  | TyVar !TyVarIdentifier
  deriving (Eq)

data LoopArray = LoopArray
  { order :: Order
  , typ :: Type
    -- ^ This should not be wrapped in the Array type.
  }
