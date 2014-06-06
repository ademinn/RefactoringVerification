module Type where

import Control.Applicative
import Data.Int
import qualified Data.List as List

data Type = ObjectType ObjectType | PrimaryType PrimaryType | NullType
    deriving (Eq, Show)

type ObjectType = String

data PrimaryType
    = TBoolean
    | TByte
    | TShort
    | TInt
    | TLong
    | TFloat
    | TDouble
    deriving (Eq, Show, Ord)

data Literal
    = LInt Int32
    | LLong Int64
    | LFloat Float
    | LDouble Double
    | LBoolean Bool
    deriving (Eq, Show)

data Ternary = Zero | Half | One
    deriving (Show, Eq, Ord)

literalType :: Literal -> PrimaryType
literalType (LInt _) = TInt
literalType (LLong _) = TLong
literalType (LFloat _) = TFloat
literalType (LDouble _) = TDouble
literalType (LBoolean _) = TBoolean

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
