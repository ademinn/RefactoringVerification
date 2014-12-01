module FJ.Expression where

import FJ.Type
import Data.List
import Control.Monad.Identity

type Identifier = String

class Holder a where
    getValue :: a b -> b

instance Holder Identity where
    getValue = runIdentity

data Typed a = Typed Type a
    deriving (Eq, Show)

instance Holder Typed where
    getValue (Typed _ x) = x

data Expression a where
    New :: (Holder a) =>  Type  -> [a (Expression a)] -> Expression a
    FieldAccess :: (Holder a) => a (Expression a) -> Identifier -> Expression a
    MethodCall :: (Holder a) => a (Expression a) -> Identifier -> [a (Expression a)] -> Expression a
    Var :: (Holder a) => Identifier -> Expression a

deriving instance (Eq (a (Expression a))) => Eq (Expression a)

type TypedExpression = Typed (Expression Typed)

instance (Show (a (Expression a))) => Show (Expression a) where
    show (New t ps) = "new " ++ t ++ "(" ++ intercalate ", " (map show ps) ++ ")"
    show (FieldAccess e fN) = show e ++ "." ++ fN
    show (MethodCall e mthN ps) = show e ++ "." ++ mthN ++ "(" ++ intercalate ", " (map show ps) ++ ")"
    show (Var v) = v
