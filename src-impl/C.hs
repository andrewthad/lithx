{-# language BangPatterns #-}
{-# language NamedFieldPuns #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language GADTs #-}
{-# language RankNTypes #-}

module C
  ( Expr(..)
  , Atom(..)
  , Type(..)
  , Case(..)
  , FunctionArgument(..)
  , VarId
  , MemberId
  , encode
  , encodeType
  ) where

import Data.Int (Int64)
import Data.Text.Short (ShortText)
import Builder (Builder)
import Data.Primitive (SmallArray,Array)

import qualified Builder as B
import qualified Data.Text.Short as T
import qualified Data.Primitive as PM

type VarId = ShortText
type MemberId = ShortText

data Type
  = SignedInt64
  | SignedInt32
  | SignedInt16
  | SignedInt8
  | SignedInt
  | UnsignedInt64
  | Bool
  | Struct !ShortText
  | Pointer !Type
  | RestrictPointer !Type

data Atom
  = Integer !Int64
  | Truth
  | Falsehood
  | Var !VarId
  | Add !Atom !Atom
  | ShiftRight !Atom !Atom
  | Subtract !Atom !Atom
  | Multiply !Atom !Atom
  | LogicalAnd !Atom !Atom
  | LessThan !Atom !Atom
  | EqualTo !Atom !Atom
  | Index !Atom !Atom
  | Arrow !Atom !MemberId
  | Dot !Atom !MemberId
  | ArrowThenDot !Atom !MemberId !MemberId
  | String !ShortText
  | Cast !Type !Atom
  | Apply !ShortText !(SmallArray Atom)
  | SizeOf !Type

data FunctionArgument = FunctionArgument
  { name :: !ShortText
  , typ :: !Type
  }

data Expr
  = Declare Type VarId
  | FunctionDeclaration
      Type -- return type
      !ShortText -- name
      !(Array FunctionArgument) -- args
      Expr -- body
  | Assign VarId Atom
  | AssignMember VarId ShortText Atom
  | AssignMemberNested VarId ShortText ShortText Atom
  | AssignMemberOffset VarId ShortText !Int64 Atom
  | DeclareAndAssign Type VarId Atom
  | Scope (Builder Expr)
  | Switch
      Atom
      (SmallArray Case)
      (Builder Expr) -- default case
  | For -- this doesn't capture everything that for loops can do
      !Type !VarId !Atom -- initialization
      !Atom -- condition
      !VarId !Atom -- update
      !(Builder Expr) -- inner loop
  | While -- Does not allow condition to appear
      !(Builder Expr)
  | IfThen
      !Atom -- condition
      !(Builder Expr) -- body
  | IfThenElse
      !Atom -- condition
      !(Builder Expr) -- true body
      !(Builder Expr) -- false body
  | Return Atom
  | Continue
  | Label !ShortText -- label name
  | Goto !ShortText -- label name
  | Break
  | Done

data Case = Case
  { pat :: !Int64
  , arm :: !(Builder Expr)
  }

encodeFunctionArgument :: FunctionArgument -> Builder ShortText
encodeFunctionArgument FunctionArgument{name,typ} =
  encodeType typ <> text " " <> B.singleton name

-- Encode C expression as text builder.
encode :: Int -> Expr -> Builder ShortText
encode !indent e = case e of
  FunctionDeclaration resTy name args def ->
    text (T.replicate indent (T.singleton ' '))
    <>
    encodeType resTy
    <>
    text " "
    <>
    text name
    <>
    text "("
    <>
    B.intercalate (text ", ") (fmap encodeFunctionArgument args)
    <>
    text ") {\n"
    <>
    encode (indent + 2) def
    <>
    text "}\n"
  Done -> mempty
  Break ->
    text (T.replicate indent (T.singleton ' '))
    <>
    text "break;\n"
  Label name ->
    text (T.replicate indent (T.singleton ' '))
    <>
    text name
    <>
    text ":;\n"
  Goto name ->
    text (T.replicate indent (T.singleton ' '))
    <>
    text "goto "
    <>
    text name
    <>
    text ";\n"
  Scope inner ->
    text (T.replicate indent (T.singleton ' '))
    <>
    char '{'
    <>
    char '\n'
    <>
    foldMap (encode (indent + 2)) inner
    <>
    text (T.replicate indent (T.singleton ' '))
    <>
    char '}'
    <>
    char '\n'
  IfThen cond body ->
    text (T.replicate indent (T.singleton ' '))
    <>
    text "if("
    <>
    encodeCAtom cond
    <>
    text ") {\n"
    <>
    foldMap (encode (indent + 2)) body
    <>
    text (T.replicate indent (T.singleton ' '))
    <>
    char '}'
    <>
    char '\n'
  IfThenElse cond onTrue onFalse ->
    text (T.replicate indent (T.singleton ' '))
    <>
    text "if("
    <>
    encodeCAtom cond
    <>
    text ") {\n"
    <>
    foldMap (encode (indent + 2)) onTrue
    <>
    text (T.replicate indent (T.singleton ' '))
    <>
    text "} else {\n"
    <>
    foldMap (encode (indent + 2)) onFalse
    <>
    text (T.replicate indent (T.singleton ' '))
    <>
    char '}'
    <>
    char '\n'
  While inner ->
    text (T.replicate indent (T.singleton ' '))
    <>
    text "while(1) {\n"
    <>
    foldMap (encode (indent + 2)) inner
    <>
    text (T.replicate indent (T.singleton ' '))
    <>
    text "}\n"
  For initTy initVar initVal cond updateVar updateVal inner ->
    text (T.replicate indent (T.singleton ' '))
    <>
    text "for("
    <>
    encodeType initTy
    <>
    char ' '
    <>
    text initVar 
    <>
    text " = "
    <>
    encodeCAtom initVal
    <>
    text "; "
    <>
    encodeCAtom cond
    <>
    text "; "
    <>
    text updateVar
    <>
    text " = "
    <>
    encodeCAtom updateVal
    <>
    text ") {\n"
    <>
    foldMap (encode (indent + 2)) inner
    <>
    text (T.replicate indent (T.singleton ' '))
    <>
    char '}'
    <>
    char '\n'
  Switch scrutinee alts def -> 
    text (T.replicate indent (T.singleton ' '))
    <>
    text "switch"
    <>
    char '('
    <>
    encodeCAtom scrutinee
    <>
    char ')'
    <>
    char ' '
    <>
    char '{'
    <>
    char '\n'
    <>
    foldMap
      (\Case{pat,arm} ->
        text (T.replicate (indent + 2) (T.singleton ' '))
        <>
        text "case " <> integer pat <> text ":;\n"
        <>
        foldMap (encode (indent + 4)) arm
      ) alts
    <>
    text (T.replicate (indent + 2) (T.singleton ' '))
    <>
    text "default:;\n"
    <>
    foldMap (encode (indent + 4)) def
    <>
    text (T.replicate indent (T.singleton ' '))
    <>
    char '}'
    <>
    char '\n'
  Assign v a ->
    text (T.replicate indent (T.singleton ' '))
    <>
    text v
    <>
    text " = "
    <>
    encodeCAtom a
    <>
    text ";\n"
  AssignMember v field a ->
    text (T.replicate indent (T.singleton ' '))
    <>
    text v
    <>
    text "->"
    <>
    text field
    <>
    text " = "
    <>
    encodeCAtom a
    <>
    text ";\n"
  AssignMemberNested v field1 field2 a ->
    text (T.replicate indent (T.singleton ' '))
    <>
    text v
    <>
    text "->"
    <>
    text field1
    <>
    text "."
    <>
    text field2
    <>
    text " = "
    <>
    encodeCAtom a
    <>
    text ";\n"
  AssignMemberOffset v field offset a ->
    text (T.replicate indent (T.singleton ' '))
    <>
    text v
    <>
    text "->"
    <>
    text field
    <>
    text "["
    <>
    text (T.pack (show offset))
    <>
    text "]"
    <>
    text " = "
    <>
    encodeCAtom a
    <>
    text ";\n"
  Declare ty v ->
    text (T.replicate indent (T.singleton ' '))
    <>
    encodeType ty
    <>
    char ' '
    <>
    text v
    <>
    text ";\n"
  DeclareAndAssign ty v a ->
    text (T.replicate indent (T.singleton ' '))
    <>
    encodeType ty
    <>
    char ' '
    <>
    text v
    <>
    text " = "
    <>
    encodeCAtom a
    <>
    text ";\n"
  Return a ->
    text (T.replicate indent (T.singleton ' '))
    <>
    text "return "
    <>
    encodeCAtom a
    <>
    text ";\n"
  Continue ->
    text (T.replicate indent (T.singleton ' '))
    <>
    text "continue;\n"

-- We lump function application in here since it binds more
-- tightly than anything else.
isSingleToken :: Atom -> Bool
isSingleToken = \case
  Integer{} -> True
  String{} -> True
  Var{} -> True
  Truth{} -> True
  Falsehood{} -> True
  SizeOf{} -> True
  _ -> False

encodeCAtom :: Atom -> Builder ShortText
encodeCAtom = \case
  Truth -> text "true"
  Falsehood -> text "false"
  Integer i -> integer i
  String t -> char '"' <> text t <> char '"' -- TODO: proper escaping
  Var v -> text v
  ShiftRight a b ->
    (if isSingleToken a then encodeCAtom a else char '(' <> encodeCAtom a <> char ')') <>
    char ' ' <> char '>' <> char '>' <> char ' ' <>
    (if isSingleToken b then encodeCAtom b else char '(' <> encodeCAtom b <> char ')')
  Add a b ->
    (if isSingleToken a then encodeCAtom a else char '(' <> encodeCAtom a <> char ')') <>
    char ' ' <> char '+' <> char ' ' <>
    (if isSingleToken b then encodeCAtom b else char '(' <> encodeCAtom b <> char ')')
  Subtract a b ->
    (if isSingleToken a then encodeCAtom a else char '(' <> encodeCAtom a <> char ')') <>
    char ' ' <> char '-' <> char ' ' <>
    (if isSingleToken b then encodeCAtom b else char '(' <> encodeCAtom b <> char ')')
  Multiply a b ->
    (if isSingleToken a then encodeCAtom a else char '(' <> encodeCAtom a <> char ')') <>
    char ' ' <> char '*' <> char ' ' <>
    (if isSingleToken b then encodeCAtom b else char '(' <> encodeCAtom b <> char ')')
  LessThan a b ->
    encodeCAtom a <>
    char ' ' <> char '<' <> char ' ' <>
    encodeCAtom b
  EqualTo a b ->
    encodeCAtom a <>
    char ' ' <> char '=' <> char '=' <> char ' ' <>
    encodeCAtom b
  LogicalAnd a b ->
    char '(' <> encodeCAtom a <> char ')' <>
    char ' ' <> char '&' <> char '&' <> char ' ' <>
    char '(' <> encodeCAtom b <> char ')'
  Index a b ->
    -- This might need parens around first arg sometimes. Figure this out.
    encodeCAtom a <> char '[' <> encodeCAtom b <> char ']'
  ArrowThenDot atom m1 m2 -> encodeCAtom atom <> text "->" <> text m1 <> char '.' <> text m2
  Arrow atom m -> encodeCAtom atom <> text "->" <> text m
  Dot atom m -> encodeCAtom atom <> text "." <> text m
  Cast ty atom -> char '(' <> encodeType ty <> char ')' <> encodeCAtom atom
  SizeOf ty -> text "sizeof(" <> encodeType ty <> char ')'
  Apply name args ->
    text name <> char '(' <>
    ( case PM.sizeofSmallArray args of
      0 -> mempty
      1 -> encodeCAtom (PM.indexSmallArray args 0)
      2 -> encodeCAtom (PM.indexSmallArray args 0) <> char ',' <>
           encodeCAtom (PM.indexSmallArray args 1)
      3 -> encodeCAtom (PM.indexSmallArray args 0) <> char ',' <>
           encodeCAtom (PM.indexSmallArray args 1) <> char ',' <>
           encodeCAtom (PM.indexSmallArray args 2)
      _ -> error "encodeCAtom: write this"
    ) <> char ')'

encodeType :: Type -> Builder ShortText
encodeType t = case t of
  Bool -> B.singleton "bool"
  UnsignedInt64 -> B.singleton "uint64_t"
  SignedInt64 -> B.singleton "int64_t"
  SignedInt32 -> B.singleton "int32_t"
  SignedInt16 -> B.singleton "int16_t"
  SignedInt8 -> B.singleton "int8_t"
  SignedInt -> B.singleton "int"
  Struct s -> B.singleton s
  Pointer c -> encodeType c <> B.singleton "*"
  RestrictPointer c -> encodeType c <> B.singleton "* restrict"

encodeTypeSuffix :: Type -> Builder ShortText
encodeTypeSuffix t = case t of
  Bool -> B.singleton "Bool"
  UnsignedInt64 -> B.singleton "U64"
  SignedInt64 -> B.singleton "S64"
  SignedInt32 -> B.singleton "S32"
  SignedInt16 -> B.singleton "S16"
  SignedInt8 -> B.singleton "S8"
  SignedInt -> B.singleton "S"
  Struct s -> B.singleton s
  Pointer target -> B.singleton "P_" <> encodeTypeSuffix target
  RestrictPointer{} -> error "encodeTypeSuffix: restrict not used here"

char :: Char -> Builder ShortText
char !c = B.singleton (T.singleton c)

text :: ShortText -> Builder ShortText
text = B.singleton

integer :: Int64 -> Builder ShortText
integer = B.singleton . T.pack . show
