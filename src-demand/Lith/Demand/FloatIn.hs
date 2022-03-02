{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}

module Lith.Demand.FloatIn
  ( run
  ) where

import Prelude hiding (id)
import Data.Foldable (foldl')
import Data.Primitive (SmallArray)
import Arithmetic.Types (Fin(Fin))
import Vector.Boxed (Vector)

import qualified Arithmetic.Types as Arithmetic
import qualified Arithmetic.Fin as Fin
import qualified Data.Primitive.Contiguous as C
import qualified Data.Primitive as PM

import qualified Identifier
import qualified Lith.Demand.Analyzed as In
import qualified Lith.Demand.Optimized as Out
import qualified Vector.Boxed as Vector
import qualified Data.Text.Short as TS

-- | This pass floats let bindings inward, closer to the use site of the
-- binding. This is able to float a binding inward multiple times in the
-- same pass.
run :: In.FunctionDeclaration -> Out.FunctionDeclaration
run In.FunctionDeclaration{name,id,argLength,argTypes,argNames,argIds,resType,definition} = Out.FunctionDeclaration
  { name
  , id
  , argLength
  , argTypes
  , argNames
  , argIds
  , resType
  , definition=goAnnoTerm definition
  }

goAnnoTerm :: In.AnnoTerm -> Out.AnnoTerm
goAnnoTerm In.AnnoTerm{typ,name,term} =
  Out.AnnoTerm{typ,name,anno=(),term=goTerm term}

-- | Making a [Decision]
--
-- Expression looks like this: let X in (let Y in (Z))
-- We have four options for each x in X:
--
-- 1. Float x into Y set. We can do this only if it appears in none
--    of the y bindings in Y.
-- 2. Float x into the top of a specific y binding. We can only do
--    this if x appears in exactly one y does not appear in Z.
--    This introduces a new LetMany.
-- 3. Leave x where it is.
-- 4. Delete x if it is not used in Y or Z.
--
-- Another case is when expression looks like this: (let X in primop(a,b))
--
-- 
data Decision
  = FloatInto
  | FloatIntoSpecific
      !Int -- index into array
  | Preserve
  | Delete

data PrimDecision arity
  = PrimFloatIntoSpecific !(Fin arity) -- argument index
  | PrimPreserve
  | PrimDelete

data DecidedBinding d = DecidedBinding
  { decision :: d
  , binding :: In.Binding
  }

decidePrim :: Arithmetic.Nat termArity -> Vector termArity In.AnnoTerm -> SmallArray In.Binding -> SmallArray (DecidedBinding (PrimDecision termArity))
decidePrim !termArity !args = C.map
  (\binding@In.Binding{id} ->
    let total = foldl'
          (\acc In.AnnoTerm{anno} -> if Identifier.member id anno then acc + 1 else acc)
          (0 :: Int)
          args
        decision = case total of
          0 -> PrimDelete
          1 -> Fin.descend termArity (errorWithoutStackTrace "decidePrim: implementation mistake") $ \fin@(Fin ix lt) acc ->
            let In.AnnoTerm{anno} = Vector.index lt args ix
             in if Identifier.member id anno
                  then PrimFloatIntoSpecific fin
                  else acc
          _ -> PrimPreserve
     in DecidedBinding{decision,binding}
  )

augmentArgs :: Arithmetic.Nat termArity -> SmallArray (DecidedBinding (PrimDecision termArity)) -> Vector termArity In.AnnoTerm -> Vector termArity Out.AnnoTerm
augmentArgs termArity decisions = Vector.imap' termArity
  (\fin original@In.AnnoTerm{typ,anno} ->
    let leaders = C.mapMaybe
          (\DecidedBinding{decision,binding} -> case decision of
            PrimFloatIntoSpecific target | target == fin -> Just binding
            _ -> Nothing
          ) decisions
        revised = case PM.sizeofSmallArray leaders of
          0 -> original
          _ -> In.AnnoTerm
            { name = TS.empty -- not sure about this name
            , typ
            , anno = foldl' (\acc In.Binding{expr=In.AnnoTerm{anno=banno}} -> banno <> acc) anno leaders
            , term = In.LetMany leaders original
            }
     in goAnnoTerm revised
  )

goLit :: In.Lit -> Out.Lit
goLit = \case
  In.LitInt i -> Out.LitInt i

goTerm :: In.Term -> Out.Term
goTerm = \case
  In.Literal lit -> Out.Literal (goLit lit)
  In.Var ident v -> Out.Var ident v
  In.CaseBool scrut onTrue onFalse ->
    Out.CaseBool (goAnnoTerm scrut) (goAnnoTerm onTrue) (goAnnoTerm onFalse)
  In.ApplyPrimitive typeArity termArity op typeArgs args ->
    Out.ApplyPrimitive typeArity termArity op typeArgs (fmap goAnnoTerm args)
  -- The LetMany case is described in [Decision].
  In.LetMany bnds0 In.AnnoTerm{typ=type0,name=name0,term=expr0} -> case expr0 of
    -- Var is simple. If it corresponds to a let binding immidiately above it,
    -- pluck out the RHS. If not, delete all the bindings above it anyway since
    -- they are not used. 
    In.Var ident v -> foldr
      (\In.Binding{id,expr=In.AnnoTerm{term}} acc -> if id == ident
        then goTerm term
        else acc
      ) (Out.Var ident v) bnds0
    In.ApplyPrimitive typeArity termArity op typeArgs args0 ->
      let decisions = decidePrim termArity args0 bnds0
          args1 = augmentArgs termArity decisions args0
          bnds1 :: SmallArray Out.Binding = C.mapMaybe
            (\DecidedBinding{decision,binding} -> case decision of
              PrimPreserve -> Just (goBinding binding)
              _ -> Nothing
            ) decisions 
          expr1 = Out.ApplyPrimitive typeArity termArity op typeArgs args1
       in case PM.sizeofSmallArray bnds1 of
            -- Drop LetMany with no bindings.
            0 -> expr1
            _ -> Out.LetMany bnds1 Out.AnnoTerm{anno=(),typ=type0,name=name0,term=expr1}
    In.LetMany bnds1 expr1@In.AnnoTerm{anno=vars1} ->
      -- Find everything that can move inward. These are bindings that
      -- are not used by any bindings in the inner let-many.
      let decisions :: SmallArray (DecidedBinding Decision) = C.map
            (\binding@In.Binding{id} ->
              let total = foldl'
                    (\acc In.Binding{expr=In.AnnoTerm{anno}} -> if Identifier.member id anno then acc + 1 else acc)
                    (0 :: Int)
                    bnds1
                  decision = case total of
                    0 -> if Identifier.member id vars1
                      then FloatInto
                      else Delete
                    1 -> if Identifier.member id vars1
                      then Preserve
                      else FloatIntoSpecific $ C.ifoldr
                        (\ix In.Binding{expr=In.AnnoTerm{anno}} acc -> if Identifier.member id anno then ix else acc
                        ) (errorWithoutStackTrace "goTerm: implementation mistake") bnds1
                    _ -> Preserve
               in DecidedBinding{decision,binding}
            ) bnds0
          -- Handles FloatIntoSpecific, must be done first for indices
          -- to match.
          bnds1' = C.imap
            (\ix bnd ->
              let leaders = C.mapMaybe
                    (\DecidedBinding{decision,binding} -> case decision of
                      FloatIntoSpecific target | target == ix -> Just binding
                      _ -> Nothing
                    ) decisions
               in case PM.sizeofSmallArray leaders of
                    0 -> bnd
                    _ -> case bnd of
                      In.Binding{id,name=bname,expr=expr@In.AnnoTerm{typ,anno}} -> In.Binding
                        { id
                        , name = bname
                        , expr = In.AnnoTerm
                          { name = bname -- I think it is right to duplicate the binding name into here.
                          , typ
                          , anno = foldl' (\acc In.Binding{expr=In.AnnoTerm{anno=banno}} -> banno <> acc) anno leaders
                          , term = In.LetMany leaders expr
                          }
                        }
            ) bnds1
          -- Handles FloatInto
          bnds1'' :: SmallArray In.Binding = bnds1' <> C.mapMaybe
            (\DecidedBinding{decision,binding} -> case decision of
              FloatInto -> Just binding
              _ -> Nothing
            ) decisions
          -- Handles Preserve
          bnds0' :: SmallArray Out.Binding = C.mapMaybe
            (\DecidedBinding{decision,binding} -> case decision of
              Preserve -> Just (goBinding binding)
              _ -> Nothing
            ) decisions 
          expr0' = goTerm (In.LetMany bnds1'' expr1)
       in case PM.sizeofSmallArray bnds0' of
            -- Drop LetMany with no bindings.
            0 -> expr0'
            _ -> Out.LetMany bnds0' Out.AnnoTerm{anno=(),typ=type0,name=name0,term=expr0'}

goBinding :: In.Binding -> Out.Binding
goBinding In.Binding{id,name,expr} = Out.Binding
  { id
  , name
  , expr = goAnnoTerm expr
  }
