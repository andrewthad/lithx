# Lith

This is a compiler for a language that supports the prenex fragment
of System F. Notably, lambda is absent in this language, but all the
usual extensions to turn lambda calculus into a programming language
are there. This includes:

* Built-in integer type
* Case on integer
* Boxed data types
* Let bindings

There is nothing particularly interesting about the language itself.
This repository is an attempt to implement a many-pass compiler for this
language. This includes a parser, typechecker, and code generator.
The novelty (to my knowledge) is a heavy reliance on the mixins that
GHC's backpack provides. There are two properties of a many-pass compiler
that make this approach attractive:

1. Data constructors of a `Term` or `Expr` type have at least a
   dozen fields whose types vary during different stages of compilation.
   We would like to be able to share a single data type across all of
   these stages. We can share implementation of ultra-generic like
   pretty printing if we do this.
2. Most stages are agnostic to the majority of fields. For example,
   during constant folding of arithmetic, we do not care how type variables
   or data constructor are represented. Mixins make it easy to enforce
   that a stage cannot access certain information.
