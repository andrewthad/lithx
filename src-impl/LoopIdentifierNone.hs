module LoopIdentifierNone
  ( LoopIdentifier
  , showAsSuffix
  ) where

import Data.Text.Short (ShortText)

type LoopIdentifier = ()

showAsSuffix :: () -> ShortText
showAsSuffix _ = mempty
 
