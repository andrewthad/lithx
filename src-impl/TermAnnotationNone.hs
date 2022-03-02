module TermAnnotationNone
  ( TermAnnotation
  , showAsSuffix
  ) where

import Data.Text.Short (ShortText)

import qualified Data.Text.Short as TS

type TermAnnotation = ()

showAsSuffix :: () -> ShortText
showAsSuffix _ = TS.empty
