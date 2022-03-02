module Lith.NamedIdentifier
  ( NamedIdentifier(..)
  ) where

import Data.Text.Short (ShortText)
import Identifier (Identifier)

data NamedIdentifier = NamedIdentifier
  { name :: !ShortText
  , id :: !Identifier
  }
