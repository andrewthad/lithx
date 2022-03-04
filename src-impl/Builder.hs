{-# language BangPatterns #-}
{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language GADTs #-}
{-# language RankNTypes #-}

module Builder
  ( Builder(..)
  , singleton
  , intercalate
  ) where

import Data.Primitive (Array)

import qualified Data.Primitive as PM

data Builder a
  = Leaf !a
  | Branch !(Builder a) !(Builder a)
  deriving stock (Functor,Foldable)

instance Semigroup (Builder a) where
  (<>) = Branch

instance Monoid a => Monoid (Builder a) where
  mempty = Leaf mempty

singleton :: a -> Builder a
singleton = Leaf

intercalate :: Monoid a => Builder a -> Array (Builder a) -> Builder a
intercalate b !xs = case PM.sizeofArray xs of
  0 -> mempty
  n ->
    let go !ix = if ix < n
          then Branch
            (Branch b (PM.indexArray xs ix))
            (go (ix + 1))
          else mempty
     in Branch
          (PM.indexArray xs 0)
          (go 1)
