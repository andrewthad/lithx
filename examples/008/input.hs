{-# language MagicHash #-}

module Input (example) where

import GHC.Exts

example :: ByteArray# -> Int#
example b =
  let go acc ix = case ix <# 10# of
        0# -> acc
        _ -> go (acc +# (indexIntArray# b ix *# 2#)) (ix +# 1#)
   in go 0# 0#
