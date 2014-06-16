module AST where

import Data.List

import Type

class WithLine a where
    lineNumber :: a -> Int

type Program = [Class]

data Class
    = Class
    { className :: String
    , classFields :: [Variable]
    , classMethods :: [Method]
    }
    deriving (Eq, Show)

data Variable
    = Variable
    { varType :: Type
    , varName :: String
    , initExpr :: Maybe Expression
    }
    deriving Eq

instance Show Variable where
    show (Variable t n e) = show t ++ " " ++ n ++ showMaybe e ++ ";"
        where
            showMaybe Nothing = ""
            showMaybe (Just e) = " = " ++ show e

data Method
    = Method
    { methodType :: MethodType
    , methodName :: String
    , methodParams :: [Parameter]
    , methodBlock :: Block
    }
    deriving Eq

instance Show Method where
    show (Method _ n p _) = n ++ "(" ++ showL p ++ ")"

showMethod :: Class -> Method -> String
showMethod (Class n _ _) mth = n ++ "." ++ show mth

data MethodType = ReturnType Type | Void | Constructor
    deriving Eq

instance Show MethodType where
    show (ReturnType rt) = show rt
    show Void = "void"
    show Constructor = "#Constructor"

data Parameter
    = Parameter
    { paramType :: Type
    , paramName :: String
    }
    deriving Eq

instance Show Parameter where
    show (Parameter t n) = show t ++ " " ++ n

type Block = [BlockStatement]

data BlockStatement = BlockVD Variable | Statement Statement
    deriving (Eq, Show)

data Statement
    = SubBlock Block
    | IfStatement If
    | WhileStatement While
    | ForStatement For
    | Return (Maybe Expression) Int
    | Break Int
    | Continue Int
    | ExpressionStatement Expression
    deriving Eq

instance Show Statement where
    show (SubBlock b) = show b
    show (IfStatement i) = show i
    show (WhileStatement w) = show w
    show (ForStatement f) = show f
    show (Return e _) = "return" ++ showMaybe e ++ ";"
        where
            showMaybe Nothing = ""
            showMaybe (Just e) = " " ++ show e
    show (Break _) = "break;"
    show (Continue _) = "continue;"
    show (ExpressionStatement e) = show e ++ ";"

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

data Expression = Expr ExpressionS Int
    deriving Eq

instance Show Expression where
    show (Expr e _) = show e

data ExpressionS
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
    deriving Eq

instance Show ExpressionS where
    show e = "(" ++ showUnbracket e ++ ")"

showUnbracket :: ExpressionS -> String
showUnbracket (Assign qn e) = show qn ++ " = " ++ show e
showUnbracket (Or e1 e2) = show e1 ++ " or " ++ show e2
showUnbracket (And e1 e2) = show e1 ++ " and " ++ show e2
showUnbracket (Equal e1 e2) = show e1 ++ " == " ++ show e2
showUnbracket (Ne e1 e2) = show e1 ++ " != " ++ show e2
showUnbracket (Lt e1 e2) = show e1 ++ " < " ++ show e2
showUnbracket (Gt e1 e2) = show e1 ++ " > " ++ show e2
showUnbracket (Le e1 e2) = show e1 ++ " <= " ++ show e2
showUnbracket (Ge e1 e2) = show e1 ++ " >= " ++ show e2
showUnbracket (Plus e1 e2) = show e1 ++ " + " ++ show e2
showUnbracket (Minus e1 e2) = show e1 ++ " - " ++ show e2
showUnbracket (Mul e1 e2) = show e1 ++ " * " ++ show e2
showUnbracket (Div e1 e2) = show e1 ++ " / " ++ show e2
showUnbracket (Mod e1 e2) = show e1 ++ " % " ++ show e2
showUnbracket (Pos e) = "+" ++ show e
showUnbracket (Neg e) = "-" ++ show e
showUnbracket (Not e) = "!" ++ show e
showUnbracket (PreInc qn) = "++" ++ show qn
showUnbracket (PreDec qn) = "--" ++ show qn
showUnbracket (PostInc qn) = show qn ++ "++"
showUnbracket (PostDec qn) = show qn ++ "--"
showUnbracket (QN qn) = show qn
showUnbracket (Literal l) = show l
showUnbracket Null = "null"

instance WithLine Expression where
    lineNumber (Expr _ l) = l

data QualifiedName = QName QualifiedNameS Int
    deriving Eq

instance Show QualifiedName where
    show (QName qn _) = show qn

data QualifiedNameS
    = FieldAccess QualifiedName String
    | MethodCall QualifiedName String [Expression]
    | Var String
    | New ObjectType [Expression]
    | This
    deriving Eq

instance WithLine QualifiedName where
    lineNumber (QName _ l) = l

instance Show QualifiedNameS where
    show (FieldAccess qn s) = show qn ++ "." ++ s
    show (MethodCall qn s ex) = show qn ++ "." ++ s ++ "(" ++ showL ex ++ ")"
    show (Var s) = s
    show (New ot ex) = "new " ++ ot ++ "(" ++ showL ex ++ ")"
    show This = "this"

showL :: (Show a) => [a] -> String
showL ex = intercalate ", " $ map show ex
