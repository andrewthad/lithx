{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}

module Lith.Primitive
  ( Primitive(..)
  , testEquality
  ) where

import Vector.Boxed (Vector)
import Data.Type.Equality ((:~:)(Refl))
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
  Multiply :: Primitive 0 2
  Negate :: Primitive 0 1
  ArrayIndex :: Primitive 1 2
  ArrayReplicate :: Primitive 1 2

deriving instance Eq (Primitive n m)

testEquality :: Primitive n0 m0 -> Primitive n1 m1 -> Maybe (m0 :~: m1)
testEquality Add Add = Just Refl
testEquality Multiply Multiply = Just Refl
testEquality Negate Negate = Just Refl
testEquality ArrayIndex ArrayIndex = Just Refl
testEquality ArrayReplicate ArrayReplicate = Just Refl
testEquality _ _ = Nothing
