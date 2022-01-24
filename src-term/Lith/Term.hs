{-# language DataKinds #-}
{-# language DuplicateRecordFields #-}
{-# language GADTs #-}
{-# language KindSignatures #-}

module Lith.Term
  ( Term(..)
  , Type(..)
  , AnnoTerm(..)
  , Binding(..)
  , Lit(..)
  , ConstructionField(..)
  ) where

import Identifier (Identifier)
import FieldIdentifier (FieldIdentifier,FieldCollection)
import FunctionIdentifier (FunctionIdentifier)
import ConIdentifier (ConIdentifier)
import TermTypeAnnotation (TermTypeAnnotation)
import FunctionTypeAnnotation (FunctionTypeAnnotation)
import Lith.Primitive (Primitive)

import Data.Functor.Const (Const)
import Vector.Boxed (Vector)
import Data.Text.Short (ShortText)
import Data.Primitive (SmallArray,Array)
import Data.Int (Int64)
import Data.Word (Word64)
import Lith.Type (Type(..))

import qualified Arithmetic.Types as Arithmetic
import qualified GHC.TypeNats as GHC

data Binding = Binding
  { id :: !Identifier
    -- ^ Internal identifier.
  , name :: !ShortText
    -- ^ Original name.
  , expr :: AnnoTerm
  }

data AnnoTerm = AnnoTerm
  { anno :: TermTypeAnnotation
  , name :: !ShortText
    -- ^ A name for this term. Used only to help C backend
    -- generate code with original names.
  , term :: !Term
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
