{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language TypeApplications #-}
module Lith.Primitive
  ( Primitive(..)
  ) where

import Vector.Boxed (Vector)
import qualified GHC.TypeNats as GHC
import qualified Data.Kind as GHC

-- Arguments are:
--
-- * Number of type arguments
-- * Number of term arguments
--
-- Why bother enforcing arity matches like this? After all, we cannot do
-- this for user-defined functions. One benefit of doing things this
-- way for primops is that we are able to doing a "is this well formed"
-- check during optimization passes. Primops are ripe targets for optimization.
-- User-defined functions, not so much.
data Primitive :: GHC.Nat -> GHC.Nat -> GHC.Type where
  Add :: Primitive 0 2
  Negate :: Primitive 0 1
  ArrayIndex :: Primitive 1 2
