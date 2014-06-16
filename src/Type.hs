module Type where

import Control.Applicative
import Data.Int
import qualified Data.List as List

data Type = ObjectType ObjectType | PrimaryType PrimaryType | NullType
    deriving Eq

instance Show Type where
    show (ObjectType ot) = ot
    show (PrimaryType pt) = show pt
    show NullType = "null"

type ObjectType = String

data PrimaryType
    = TBoolean
    | TByte
    | TShort
    | TInt
    | TLong
    | TFloat
    | TDouble
    deriving (Eq, Ord)

instance Show PrimaryType where
    show TBoolean = "boolean"
    show TByte = "byte"
    show TShort = "short"
    show TInt = "int"
    show TLong = "long"
    show TFloat = "float"
    show TDouble = "double"

data Literal
    = LBoolean Bool
    | LByte Int8
    | LShort Int16
    | LInt Int32
    | LLong Int64
    | LFloat Float
    | LDouble Double
    deriving Eq

instance Show Literal where
    show (LBoolean b) = show b
    show (LByte b) = show b
    show (LShort s) = show s
    show (LInt i) = show i
    show (LLong l) = show l
    show (LFloat f) = show f
    show (LDouble d) = show d
    

data Ternary = Zero | Half | One
    deriving (Show, Eq, Ord)

literalType :: Literal -> PrimaryType
literalType (LBoolean _) = TBoolean
literalType (LByte _) = TByte
literalType (LShort _) = TShort
literalType (LInt _) = TInt
literalType (LLong _) = TLong
literalType (LFloat _) = TFloat
literalType (LDouble _) = TDouble

inferPrimary :: PrimaryType -> PrimaryType -> Maybe PrimaryType
inferPrimary TBoolean TBoolean = Just TBoolean
inferPrimary TBoolean _ = Nothing
inferPrimary _ TBoolean = Nothing
inferPrimary t1 t2 = case compare t1 t2 of
    GT -> Just t1
    EQ -> Just t1
    LT -> Just t2

infer :: Type -> Type -> Maybe Type
infer (PrimaryType t1) (PrimaryType t2) = PrimaryType <$> inferPrimary t1 t2
infer o@(ObjectType _) NullType = Just o
infer NullType o@(ObjectType _) = Just o
infer t1 t2 = if t1 == t2 then Just t1 else Nothing

castPrimary :: PrimaryType -> PrimaryType -> Bool
castPrimary TBoolean TBoolean = True
castPrimary TBoolean _ = False
castPrimary _ TBoolean = False
castPrimary t1 t2 = t1 <= t2

cast :: Type -> Type -> Bool
cast (PrimaryType t1) (PrimaryType t2) = castPrimary t1 t2
cast (PrimaryType _) _ = False
cast _ (PrimaryType _) = False
cast _ NullType = error "error in compiler"
cast NullType _ = True
cast t1 t2 = t1 == t2

canCast :: Type -> Type -> Ternary
canCast from to
    | from == to = One
    | cast from to = Half
    | otherwise = Zero

castTypeLists :: [Type] -> [Type] -> Ternary
castTypeLists from to
    | length from /= length to = Zero
    | null from = One
    | otherwise = List.minimum $ List.zipWith canCast from to
