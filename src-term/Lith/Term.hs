{-# language DataKinds #-}
{-# language DuplicateRecordFields #-}
{-# language GADTs #-}
{-# language KindSignatures #-}

module Lith.Term
  ( Term(..)
  , Type(..)
  , LoopArray(..)
  , AnnoTerm(..)
  , Binding(..)
  , OrderedExpr(..)
  , LoopBody(..)
  , LoopEpilogue(..)
  , LoopDeclarations(..)
  , NamedIdentifier(..)
  , Lit(..)
  , ConstructionField(..)
  , FunctionDeclaration(..)
  ) where

import Identifier (Identifier)
import LoopIdentifier (LoopIdentifier)
import FieldIdentifier (FieldIdentifier,FieldCollection)
import FunctionIdentifier (FunctionIdentifier)
import ConIdentifier (ConIdentifier)
import TermTypeAnnotation (TermTypeAnnotation)
import TermAnnotation (TermAnnotation)
import FunctionTypeAnnotation (FunctionTypeAnnotation)
import Lith.Primitive (Primitive)
import Order (Order)

import Vector.Boxed (Vector)
import Data.Text.Short (ShortText)
import Data.Primitive (SmallArray,Array)
import Data.Int (Int64)
import Data.Word (Word64)
import Lith.NamedIdentifier (NamedIdentifier(..))
import Lith.Type (Type(..),LoopArray(..))

import qualified Arithmetic.Types as Arithmetic
import qualified GHC.TypeNats as GHC

-- | A single let-bound expression along with the identifier that it
-- is bound to.
data Binding = Binding
  { id :: !Identifier
    -- ^ Internal identifier.
  , name :: !ShortText
    -- ^ Original name.
  , expr :: AnnoTerm
    -- ^ Expression, includes the type.
  }

-- | A term, possibly annotated with:
--
-- 1. The type.
-- 2. Phase specific annotations. For example, an annotation tracking
--    the which let-bound expressions are used.
data AnnoTerm = AnnoTerm
  { typ :: TermTypeAnnotation
    -- ^ The type of the term, or unit if we are in a phase where
    -- type information is not available.
  , anno :: TermAnnotation
    -- ^ A phase-specific annotation.
  , name :: !ShortText
    -- ^ A name for this term. Used only to help C backend
    -- generate code with original names.
    -- TODO: Stop hard-coding this to ShortText
  , term :: !Term
    -- ^ The term.
  }

data FunctionDeclaration = forall (n :: GHC.Nat). FunctionDeclaration
  { name :: !ShortText
  , id :: !FunctionIdentifier
  , argLength :: !(Arithmetic.Nat n)
  , argTypes :: !(Vector n Type)
  , argNames :: !(Vector n ShortText)
  , argIds :: !(Vector n Identifier)
    -- All of these arg fields should have the same size.
  , resType :: Type
  , definition :: AnnoTerm
    -- ^ TODO: move definition out. It is probably useful to be able
    -- to talk about a function declaration without having the definition
    -- available.
  }

data Term
  = LetMany !(SmallArray Binding) AnnoTerm
  | Literal Lit
  | Var
      !Identifier
      !ShortText -- original name
  | Project AnnoTerm !FieldIdentifier
  | forall (typeArity :: GHC.Nat) (termArity :: GHC.Nat). ApplyPrimitive
      !(Arithmetic.Nat typeArity)
      !(Arithmetic.Nat termArity)
      !(Primitive typeArity termArity)
      !(FunctionTypeAnnotation (Vector typeArity Type))
      !(Vector termArity AnnoTerm)
  | CaseBool
      AnnoTerm -- scrutinee
      AnnoTerm -- on true
      AnnoTerm -- on false
  | Apply
      !FunctionIdentifier
      !(FunctionTypeAnnotation (Array Type)) -- cannot enforce arity agreement
      !(Array AnnoTerm)
  | forall input output st monoid. For
      !LoopIdentifier -- loop identifier
      !ShortText -- original name
      !(Arithmetic.Nat input)
      !(Arithmetic.Nat output)
      !(Arithmetic.Nat st)
      (LoopDeclarations input output st monoid)
      (LoopBody input st)
      (LoopEpilogue st output monoid)
  | Continue
      !LoopIdentifier
      !ShortText -- original name of loop
      !(Array AnnoTerm) -- output state
      !(Array AnnoTerm) -- output elements
      !(Array AnnoTerm) -- output monoid values

data LoopBody input st = LoopBody
  { indexName :: !ShortText
  , indexId :: !Identifier
  , inputArrays :: !(Vector input NamedIdentifier)
    -- ^ TODO: rename inputArrays. It should be elements instead of arrays.
  , state :: !(Vector st NamedIdentifier)
  , expr :: !AnnoTerm
    -- ^ This expression must call continue or must jump.
  }

data LoopEpilogue st output monoid = LoopEpilogue
  { state :: !(Vector st NamedIdentifier)
  , outputArrays :: !(Vector output NamedIdentifier)
  , monoids :: ()
  , expr :: !AnnoTerm
  }

data LoopDeclarations input output st monoid = LoopDeclarations
  { length :: AnnoTerm
  , inputArrays :: !(Vector input OrderedExpr)
  , initialStates :: !(Vector st AnnoTerm)
  , outputArrays :: !(Vector output LoopArray)
  , monoids :: ()
  }

data OrderedExpr = OrderedExpr
  { order :: Order
  , expr :: AnnoTerm
  }

-- data ArrayBinding = ArrayBinding
--   { order :: Order
--   , id :: !Identifier
--   , name :: !ShortText
--   , array :: AnnoTerm
--     -- ^ Must have type Array
--   } 

data Lit
  = LitInt !Int64
  | LitBits !Word64
  | LitTrue
  | LitFalse
  | LitArray (FunctionTypeAnnotation Type) !(SmallArray AnnoTerm)
  | LitCon
      !ConIdentifier
      !ShortText -- what was written in the source code
      !(FunctionTypeAnnotation (Array Type))
      !(FieldCollection ConstructionField)

data ConstructionField = ConstructionField
  { name :: !ShortText
  , expr :: AnnoTerm 
  }
