module FJ.ParseTree where

import FJ.Common

type Program = [Class]

data Class
    = Class
    { clsName :: String
    , clsBase :: Identifier
    , clsFields :: [Variable]
    , clsConstructors :: [Constructor]
    , clsMethods :: [Method]
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
    , mthExpr :: RawExpression
    }
    deriving (Eq, Show)

data ConsBlock
     = ConsBlock
     { superCallParams :: [Identifier]
     , assignList :: [Assign]
     }
     deriving (Eq, Show)

data Assign
    = Assign
    { field :: Identifier
    , value :: Identifier
    }
    deriving (Eq, Show)

--data Expression
--    = New Type [Expression]
--    | FieldAccess Expression Identifier
--    | MethodCall Expression Identifier [Expression]
--    | Var Identifier
--    deriving Eq
--
--instance Show Expression where
--    show (New t ps) = "new " ++ t ++ "(" ++ intercalate ", " (map show ps) ++ ")"
--    show (FieldAccess e fN) = show e ++ "." ++ fN
--    show (MethodCall e mthN ps) = show e ++ "." ++ mthN ++ "(" ++ intercalate ", " (map show ps) ++ ")"
--    show (Var v) = v
--
--type Identifier = String
