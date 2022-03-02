{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}

module Lith.Demand.DeadCodeElimination
  ( run
  ) where

import Prelude hiding (id)

import qualified Data.Primitive.Contiguous as C

import qualified Identifier
import qualified Lith.Demand.Analyzed as In
import qualified Lith.Demand.Optimized as Out

-- | This pass deletes unused let bindings. If the body of
-- let-bndrs-equal-exprs-in-body does not reference a binder, that binder
-- may be deleted.
--
-- The only interesting case is let bindings. For everything else, this
-- just performs a standard traversal.
--
-- Note: This implementation of dead code elimination performs poorly when
-- there are long chains of identifiers whose deletion unlocks the deletion
-- of the next one.
--
-- TODO: All the boring code still needs to be written.
-- TODO: This is best implemented as a bottom-up pass. It should just be
--   done at the same time that annotations are added.
run :: In.AnnoTerm -> Out.AnnoTerm
run = goAnnoTerm

goAnnoTerm :: In.AnnoTerm -> Out.AnnoTerm
goAnnoTerm In.AnnoTerm{typ,name,term} =
  Out.AnnoTerm{typ,name,anno=(),term=goTerm term}

goTerm :: In.Term -> Out.Term
goTerm = \case
  In.Var ident v -> Out.Var ident v
  In.LetMany bnds expr@In.AnnoTerm{anno} -> Out.LetMany
    (C.mapMaybe
      (\In.Binding{id,name,expr=bound} -> if Identifier.member id anno
        then Just Out.Binding{id,name,expr=goAnnoTerm bound}
        else Nothing
      )
      bnds
    )
    (goAnnoTerm expr)
    
