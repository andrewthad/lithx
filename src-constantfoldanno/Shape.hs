{-# language BangPatterns #-}
{-# language NamedFieldPuns #-}

-- This will need to be moved elsewhere once the Shape type becomes
-- more sophisticated. In particular, we do not data constructors
-- at the moment.
module Shape
  ( Shape(..)
  , Alias(..)
  , Metadata(..)
  , removeFromContext
  ) where

import Prelude hiding (id)

import Data.Int (Int64)
import Data.Word (Word64)
import Data.IntMap.Strict (IntMap)
import Data.Text.Short (ShortText)

import Identifier (Identifier)

data Metadata = Metadata
  { alias :: Maybe Alias
    -- ^ Any variable that is known to be the same as this expression.
    -- TODO: make this track all matching variables instead. Sometimes,
    -- a binder goes out of scope, but another match is available.
  , shape :: Shape
    -- ^ The actual shape
  }

data Alias = Alias
  { id :: !Identifier
  , name :: !ShortText
  }

data Shape
  = ShapeInt !Int64
  | ShapeBits !Word64
  | ShapeArray
      !(IntMap Metadata) -- any known elements
  | ShapeTrue
  | ShapeFalse
  | ShapeUnknown

-- | If something goes out of scope, all references to it in the metadata
-- must be removed.
removeFromContext :: Identifier -> Metadata -> Metadata
removeFromContext !ident Metadata{alias,shape} =
  let newAlias = case alias of
        Nothing -> Nothing
        Just Alias{id} -> if id == ident
          then Nothing
          else alias
      newShape = case shape of
        ShapeInt{} -> shape
        ShapeBits{} -> shape
        ShapeTrue{} -> shape
        ShapeFalse{} -> shape
        ShapeUnknown{} -> shape
        ShapeArray m -> ShapeArray (fmap (removeFromContext ident) m)
   in Metadata{alias=newAlias,shape=newShape}
