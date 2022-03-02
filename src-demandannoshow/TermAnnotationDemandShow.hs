module TermAnnotationDemandShow
  ( showAsSuffix
  ) where

import Identifier (IdentifierSet)
import Data.Text.Short (ShortText)
import qualified Identifier

showAsSuffix :: IdentifierSet -> ShortText
showAsSuffix = Identifier.showSetAsSuffix
