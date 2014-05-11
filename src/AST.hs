module AST where

import Data.Int

type Program = [Class]

data Class
    = Class
    { className :: String
    , classMembers :: [Member]
    }
    deriving (Eq, Show)

data Member = MemberVD Variable | MemberMD Method
    deriving (Eq, Show)

data Variable
    = Variable
    { varType :: Type
    , varName :: String
    , initExpr :: Maybe Expression
    }
    deriving (Eq, Show)

data Method
    = Method
    { methodType :: MethodType
    , methodName :: String
    , methodParams :: [Parameter]
    , methodBlock :: Block
    }
    deriving (Eq, Show)

data MethodType = ReturnType Type | Void | Constructor
    deriving (Eq, Show)

data Parameter
    = Parameter
    { paramType :: Type
    , paramName :: String
    }
    deriving (Eq, Show)

type Block = [BlockStatement]

data BlockStatement = BlockVD Variable | Statement Statement
    deriving (Eq, Show)

data Statement
    = SubBlock Block
    | IfStatement If
    | WhileStatement While
    | ForStatement For
    | Return (Maybe Expression)
    | Break
    | Continue
    | ExpressionStatement Expression
    deriving (Eq, Show)

data If
    = If 
    { ifCondition :: Expression
    , consequent :: Statement
    , alternative :: Maybe Statement
    }
    deriving (Eq, Show)

data While
    = While
    { whileCondition :: Expression
    , whileStatement :: Statement
    }
    deriving (Eq, Show)

data For
    = For
    { forInit :: ForInit
    , forCondition :: Expression
    , forIncrement :: [Expression]
    , forStatement :: Statement
    }
    deriving (Eq, Show)

data ForInit = ForInitVD Variable | ForInitEL [Expression]
    deriving (Eq, Show)

data Expression
    = Assign QualifiedName Expression
    | Or Expression Expression
    | And Expression Expression
    | Equal Expression Expression
    | Ne Expression Expression
    | Lt Expression Expression
    | Gt Expression Expression
    | Le Expression Expression
    | Ge Expression Expression
    | Plus Expression Expression
    | Minus Expression Expression
    | Mul Expression Expression
    | Div Expression Expression
    | Mod Expression Expression
    | Pos Expression
    | Neg Expression
    | Not Expression
    | PreInc QualifiedName
    | PreDec QualifiedName
    | PostInc QualifiedName
    | PostDec QualifiedName
    | QN QualifiedName
    | Literal Literal
    | Null
    deriving (Eq, Show)

data QualifiedName
    = FieldAccess QualifiedName String
    | MethodCall QualifiedName String [Expression]
    | FieldOrVar String
    | New ObjectType [Expression]
    | This
    deriving (Eq, Show)
 
data Type = ObjectType ObjectType | PrimaryType PrimaryType
    deriving (Eq, Show)

data ObjectType = CustomType String
    deriving (Eq, Show)

data PrimaryType
    = TBoolean
    | TByte
    | TShort
    | TInt
    | TLong
    | TFloat
    | TDouble
    deriving (Eq, Show)

data Literal
    = LInt Int32
    | LLong Int64
    | LFloat Float
    | LDouble Double
    | LBoolean Bool
    deriving (Eq, Show)
