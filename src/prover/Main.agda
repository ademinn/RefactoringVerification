module Main where
open import IO
open import Data.String
open import Data.List

{-# IMPORT AST #-}

{-# COMPILED_TYPE String AST.Type #-}
{-# COMPILED_TYPE String AST.Identifier #-}

data Variable : Set where
  V : String → String → Variable

{-# COMPILED_DATA Variable AST.Variable AST.Variable #-}

data Expression : Set where
  New : String → List Expression → Expression
  FieldAccess : String → Expression → String → Expression
  MethodCall : String → Expression → String → List Expression → Expression
  Var : String → String → Expression

{-# COMPILED_DATA Expression AST.Expression AST.New AST.FieldAccess AST.MethodCall AST.Var #-}

data Method : Set where
  Mth : String → List Variable → Expression → Method

{-# COMPILED_DATA Method AST.Method AST.Method #-}

data Constructor : Set where
  Cons : List Variable → Constructor

{-# COMPILED_DATA Constructor AST.Constructor AST.Constructor #-}

data Pair (A B : Set) : Set where
  P : A → B → Pair A B

{-# COMPILED_DATA Pair (,) (,) #-}

data SimpleMap (A B : Set) : Set where
  SM : List (Pair A B) → SimpleMap A B

{-# COMPILED_DATA SimpleMap AST.SimpleMap AST.SimpleMap #-}

data Class : Set where
  Cls : String → SimpleMap String String → Constructor → SimpleMap String Method → Class

{-# COMPILED_DATA Class AST.Class AST.Class #-}

data Program : Set where
  Prog : SimpleMap String Class → Program

{-# COMPILED_DATA Program AST.Program AST.Program #-}

postulate
  parseUnsafe : String → Program
  showProgram : Program → String

{-# COMPILED parseUnsafe AST.parseUnsafe #-}
{-# COMPILED showProgram AST.showProgram #-}

main = run (putStrLn (showProgram (parseUnsafe "class A extends Object { A() {super();} }")))
