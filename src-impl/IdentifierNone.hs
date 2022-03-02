module IdentifierNone
  ( Identifier
  , showAsSuffix
  ) where

import Data.Text.Short (ShortText)

type Identifier = ()

showAsSuffix :: () -> ShortText
showAsSuffix _ = mempty
