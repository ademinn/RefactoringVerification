module FJ.ParseTree
( module FJ.Type
, module FJ.ParseTree
) where

import FJ.Type
import Data.List

type Program = [Class]

data Class
    = Class
    { clsName :: String
    , clsFields :: [Variable]
    , clsConstructors :: [Constructor]
    , clsMethods :: [Method]
    }
    deriving (Eq, Show)

data Variable
    = Variable
    { varType :: Type
    , varName :: String
    }
    deriving (Eq, Show)

data Constructor
    = Constructor
    { consName :: String
    , consParams :: [Variable]
    , consBlock :: ConsBlock
    }
    deriving (Eq, Show)

data Method
    = Method
    { mthType :: Type
    , mthName :: String
    , mthParams :: [Variable]
    , mthExpr :: Expression
    }
    deriving (Eq, Show)

type ConsBlock = [Assign]

data Assign
    = Assign
    { field :: Identifier
    , value :: Identifier
    }
    deriving (Eq, Show)

data Expression
    = New Type [Expression]
    | FieldAccess Expression Identifier
    | MethodCall Expression Identifier [Expression]
    | Var Identifier
    deriving Eq

instance Show Expression where
    show (New t ps) = "new " ++ t ++ "(" ++ intercalate ", " (map show ps) ++ ")"
    show (FieldAccess e fN) = show e ++ "." ++ fN
    show (MethodCall e mthN ps) = show e ++ "." ++ mthN ++ "(" ++ intercalate ", " (map show ps) ++ ")"
    show (Var v) = v

type Identifier = String
