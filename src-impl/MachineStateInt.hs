module MachineStateInt
  ( M
  , failure
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT)

type M = StateT Int (Either String)

failure :: String -> M a
failure = lift . Left
