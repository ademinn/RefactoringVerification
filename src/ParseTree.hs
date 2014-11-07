module ParseTree
( module Type
, module ParseTree
) where

import Type

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
    deriving (Eq, Show)

type Identifier = String
