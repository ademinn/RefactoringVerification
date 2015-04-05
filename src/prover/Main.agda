module Main where
open import IO
open import Data.String
open import Data.List
open import Data.Bool

{-# IMPORT AST #-}

{-# COMPILED_TYPE String AST.Type #-}
{-# COMPILED_TYPE String AST.Identifier #-}

data Variable : Set where
  V : String → String → Variable

varName : Variable → String
varName (V _ name) = name

{-# COMPILED_DATA Variable AST.Variable AST.Variable #-}

data Expression : Set where
  New : String → List Expression → Expression
  FieldAccess : String → Expression → String → Expression
  MethodCall : String → Expression → String → List Expression → Expression
  Var : String → String → Expression

exprType : Expression → String
exprType (New t _) = t
exprType (FieldAccess t _ _) = t
exprType (MethodCall t _ _ _) = t
exprType (Var t _) = t

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
  getValue : {A B : Set} → A → SimpleMap A B → B

{-# COMPILED parseUnsafe AST.parseUnsafe #-}
{-# COMPILED showProgram AST.showProgram #-}

infixr 5 _++l_

_++l_ : ∀ {a} {A : Set a} → List A → List A → List A
xs ++l ys = Data.List._++_ xs ys

{-# NO_TERMINATION_CHECK #-}
subst : Pair String Expression → Expression → Expression
subst substExpr (New t ps) = New t (map (subst substExpr) ps)
subst substExpr (FieldAccess t e f) = FieldAccess t (subst substExpr e) f
subst substExpr (MethodCall t e m ps) = MethodCall t (subst substExpr e) m (map (subst substExpr) ps)
subst (P name expr) (Var t vName) = if name == vName then expr else (Var t vName)

getMethod : Program → String → String → Method
getMethod (Prog classes) clsName mthName with getValue clsName classes
... | Cls _ _ _ methods = getValue mthName methods

reduceInvk : Program → Expression → String → List Expression → Expression
reduceInvk p e mthName params with getMethod p (exprType e) mthName
... | Mth _ vars body = foldr subst body ((P "this" e) ∷ (zipWith P (map varName vars) params))

data Reduce {Pr : Program} : Expression → Expression → Set where
  RInvk : ∀ {t consParams mthType mthName mthParams} → Reduce {Pr} (MethodCall mthType (New t consParams) mthName mthParams) (reduceInvk Pr (New t consParams) mthName mthParams)
  RCField : ∀ {e₁ e₂ t f} → Reduce {Pr} e₁ e₂ → Reduce {Pr} (FieldAccess t e₁ f) (FieldAccess t e₂ f)
  RCInvk : ∀ {e₁ e₂ t m p} → Reduce {Pr} e₁ e₂ → Reduce {Pr} (MethodCall t e₁ m p) (MethodCall t e₂ m p)
  RCInvkArg : ∀ {e e₁ e₂ t m p₁ p₂} → Reduce {Pr} e₁ e₂ → Reduce {Pr} (MethodCall t e m (p₁ ++l [ e₁ ] ++l p₂)) (MethodCall t e m (p₁ ++l [ e₂ ] ++l p₂))
  RCNewArg : ∀ {e₁ e₂ t p₁ p₂} → Reduce {Pr} e₁ e₂ → Reduce {Pr} (New t (p₁ ++l [ e₁ ] ++l p₂)) (New t (p₁ ++l [ e₂ ] ++l p₂))

main = run (putStrLn (showProgram (parseUnsafe "class A extends Object { A() {super();} }")))
