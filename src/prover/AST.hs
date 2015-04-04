module AST where

import qualified FJ
import Data.Map (assocs)
import Control.Applicative

type Type = FJ.Identifier

type Identifier = FJ.Identifier

data SimpleMap a b = SimpleMap [(a, b)] deriving Show

data Program = Program (SimpleMap Identifier Class) deriving Show

data Variable = Variable Type Identifier deriving Show

data Class
    = Class
    { base :: Identifier
    , fields :: SimpleMap Identifier Type
    , constructor :: Constructor
    , methods :: SimpleMap Identifier Method
    }
    deriving Show

data Constructor = Constructor [Variable] deriving Show

data Method
    = Method
    { mthType :: Type
    , params :: [Variable]
    , body :: Expression
    }
    deriving Show

data Expression
    = New Type [Expression]
    | FieldAccess Type Expression Identifier
    | MethodCall Type Expression Identifier [Expression]
    | Var Type Identifier
    deriving Show

fromLibExpression :: FJ.TypedExpression -> Expression
fromLibExpression (FJ.Typed _ (FJ.New t exs)) = New t (map fromLibExpression exs)
fromLibExpression (FJ.Typed t (FJ.FieldAccess e i)) = FieldAccess t (fromLibExpression e) i
fromLibExpression (FJ.Typed t (FJ.MethodCall e i exs)) = MethodCall t (fromLibExpression e) i (map fromLibExpression exs)
fromLibExpression (FJ.Typed t (FJ.Var i)) = Var t i

fromLibVariable :: FJ.Variable -> Variable
fromLibVariable (FJ.Variable t n) = Variable t n

fromLibMethod :: FJ.Method -> Method
fromLibMethod (FJ.Method mt mp mb) = Method mt (map fromLibVariable mp) $ fromLibExpression mb


fromLibClass :: FJ.Class -> Class
fromLibClass (FJ.Class b fs cons ms) = Class b (SimpleMap $ assocs fs) (Constructor $ map fromLibVariable cons) $ SimpleMap $ map mapClsMth $ assocs ms
    where
        mapClsMth (name, mth) = (name, fromLibMethod mth)

fromLibProgram :: FJ.Program -> Program
fromLibProgram p = Program $ SimpleMap $ map mapProgCls $ assocs p
    where
        mapProgCls (name, cls) = (name, fromLibClass cls)

parse :: String -> Either String Program
parse s = fromLibProgram <$> FJ.parse s

parseUnsafe :: String -> Program
parseUnsafe s = case parse s of
    Right p -> p
    otherwise -> undefined

showProgram :: Program -> String
showProgram = show
