module Lith.Type
  ( Type(..)
  ) where

import Data.Text.Short (ShortText)
import Data.Primitive (Array)
import ConIdentifier (ConIdentifier)
import TyVarIdentifier (TyVarIdentifier)

data Type
  = Int
  | Bits
  | Bool
  | Array Type
  | Box
      !ConIdentifier -- type constructor
      !ShortText -- what was written in the source code
      !(Array Type) -- type arguments
  | TyVar !TyVarIdentifier
  deriving (Eq)
