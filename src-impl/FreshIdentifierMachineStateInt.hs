module FreshIdentifierMachineStateInt
  ( freshIdentifier
  ) where

import MachineStateInt (M)
import Control.Monad.Trans.State.Strict (get,put)

freshIdentifier :: M Int
freshIdentifier = do
  i <- get
  put (i + 1)
  pure i
