module MachineEither
  ( M
  , failure
  ) where

type M = Either String

failure :: String -> M a
failure = Left

