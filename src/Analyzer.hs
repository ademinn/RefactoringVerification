module Analyzer where

import Control.Applicative
import qualified Control.Monad as CM
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Monad.Error
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Foldable as Foldable

import Scope
import AST
import Type

type FieldsIndexes = Map.Map String (Map.Map String Int)

data SymbolTable
    = SymbolTable
    { fieldsIndexes :: FieldsIndexes
    , stScope :: Scope
    } deriving Show

type SemanticError = String

data AnalyzerState
    = AnalyzerState
    { maybeProgram :: Maybe Program
    , symbolTable :: SymbolTable
    , currentMaybeClass :: Maybe Class
    , currentMaybeMethod :: Maybe Method
    } deriving Show

type AnalyzerT m a = StateT AnalyzerState (WriterT [SemanticError] m) a

type Analyzer a = AnalyzerT Identity a

type AnalyzerError a = AnalyzerT (ErrorT String Identity) a

data ValueType = LValue | RValue
    deriving (Show, Eq, Ord)

class Analyzable a where
    analyze :: a -> Analyzer ()

instance Scoped AnalyzerState where
    getScope = stScope . symbolTable
    setScope st sc = st { symbolTable = (symbolTable st) { stScope = sc } }

defaultSymbolTable :: SymbolTable
defaultSymbolTable = SymbolTable Map.empty []

defaultAnalyzerState :: AnalyzerState
defaultAnalyzerState = AnalyzerState Nothing defaultSymbolTable Nothing Nothing

program :: AnalyzerState -> Program
program = fromJust . maybeProgram

currentClass :: AnalyzerState -> Class
currentClass = fromJust . currentMaybeClass

currentMethod :: AnalyzerState -> Method
currentMethod = fromJust . currentMaybeMethod

analyzeMaybe :: Analyzable a => Maybe a -> Analyzer ()
analyzeMaybe = Foldable.mapM_ analyze 

newError :: Monad m => SemanticError -> AnalyzerT m ()
newError err = tell [err]

stopAnalyzer :: SemanticError -> AnalyzerError a
stopAnalyzer err = do
    newError err
    throwError err

findType :: (Monad m) => ObjectType -> AnalyzerT m (Maybe Class)
findType typeName = do
    classes <- gets program
    return $ List.find (\cls -> className cls == typeName) classes

isTypeDefined :: Type -> Analyzer Bool
isTypeDefined (ObjectType typeName) = isJust <$> findType typeName
isTypeDefined _ = return True

checkTypeDefined :: Type -> Analyzer Bool
checkTypeDefined t = do
    defined <- isTypeDefined t
    CM.unless defined $ newError $ "Type " ++ show t ++ " undefined"
    return defined

checkClassFields :: Class -> Analyzer ()
checkClassFields cls = do
    CM.mapM_ (checkTypeDefined . varType) (classFields cls)
    return ()

getLocalVar :: String -> Analyzer (Maybe VarSymbol)
getLocalVar n = do
    var <- lookupLocalVar n
    CM.unless (isJust var) $ newError $ "Variable " ++ n ++ " not defined"
    return var

addLocalVar :: Type -> String -> Bool -> Analyzer ()
addLocalVar t n i = do
    res <- newLocalVar t n i
    Foldable.forM_ res newError

checkClass :: String -> AnalyzerError Class
checkClass clsName = do
    mcls <- findType clsName
    case mcls of
        Just cls -> return cls
        Nothing -> stopAnalyzer $ "No class with name " ++ clsName

filterMethod :: Method -> [Type] -> Ternary
filterMethod ms params = castTypeLists params $ map paramType $ methodParams ms

checkMethod :: ObjectType -> String -> [Type] -> (Method -> Bool) -> AnalyzerError MethodType
checkMethod typeName mth prms f = do
    cls <- checkClass typeName
    let methods = filter (\m -> (methodName m == mth) && (filterMethod m prms /= Zero) && f m) $ classMethods cls
        (ones, halfs) = List.partition (\m -> filterMethod m prms == One) methods
    if length ones == 1
        then
            return . methodType . head $ ones
        else
            case compare (length halfs) 1 of
                GT -> stopAnalyzer $ "Ambiguous method call " ++ mth
                LT -> stopAnalyzer "None method match"
                EQ -> return . methodType . head $ halfs

analyzeQualifiedName :: QualifiedName -> AnalyzerError MethodType
analyzeQualifiedName (FieldAccess qn field) = do
    t <- analyzeQualifiedName qn
    case t of
        ReturnType (ObjectType ot) -> checkField ot field
        Constructor -> error "error in analyzer"
        t' -> stopAnalyzer $ show t' ++ " has no fields"
    where
        checkField typeName fieldName = do
            cls <- checkClass typeName
            let mfl = List.find (\f -> varName f == fieldName) $ classFields cls
            case mfl of
                Just fl -> return . ReturnType . varType $ fl
                Nothing -> stopAnalyzer $ "class " ++ typeName ++ " has no field " ++ fieldName

analyzeQualifiedName (MethodCall qn method params) = do
    t <- analyzeQualifiedName qn
    paramTypes <- mapM analyzeReturnExpression params
    case t of
        ReturnType (ObjectType ot) -> checkMethod ot method paramTypes (\m -> methodType m /= Constructor)
        Constructor -> error "error in analyzer"
        t' -> stopAnalyzer $ show t' ++ " has no methods"
                            
analyzeQualifiedName (Var var) = do
    vs <- lookupLocalVar var
    case vs of
        Just v -> return . ReturnType . vsType $ v
        Nothing -> analyzeQualifiedName $ FieldAccess This var

analyzeQualifiedName (New typeName params) = do
    paramTypes <- mapM analyzeReturnExpression params
    void $ checkMethod typeName typeName paramTypes (\m -> methodType m == Constructor)
    return . ReturnType . ObjectType $ typeName
analyzeQualifiedName This = ReturnType <$> ObjectType <$> className <$> gets currentClass

analyzeReturnQualifiedName :: QualifiedName -> AnalyzerError Type
analyzeReturnQualifiedName qn = do
    q <- analyzeQualifiedName qn
    case q of
        ReturnType t -> return t
        _ -> stopAnalyzer "Qualified name has no value"

getValueType :: QualifiedName -> ValueType
getValueType (FieldAccess _ _) = LValue
getValueType (Var _) = LValue
getValueType _ = RValue

inferType :: Type -> Type -> AnalyzerError Type
inferType t1 t2 = do
    let mt = infer t1 t2
    case mt of
        Just t -> return t
        Nothing -> stopAnalyzer "Can not infer types"

castType :: Type -> Type -> AnalyzerError ()
castType t1 t2 = CM.unless (cast t1 t2) $ stopAnalyzer $ "Can't cast " ++ show t1 ++ " to " ++ show t2

checkBoolOp :: Expression -> Expression -> AnalyzerError MethodType
checkBoolOp e1 e2 = do
    et1 <- analyzeReturnExpression e1
    et2 <- analyzeReturnExpression e2
    case (et1, et2) of
        (PrimaryType TBoolean, PrimaryType TBoolean) -> return . ReturnType . PrimaryType $ TBoolean
        (PrimaryType TBoolean, _) -> stopAnalyzer $ "Not boolean expression: " ++ show e2
        (_, PrimaryType TBoolean) -> stopAnalyzer $ "Not boolean expression: " ++ show e1
        _ -> stopAnalyzer $ "Not boolean expressions: " ++ show e1 ++ ", " ++ show e2

checkNumericType :: Type -> AnalyzerError MethodType
checkNumericType t = case t of
        PrimaryType TBoolean -> stopAnalyzer "Wrong expression type: boolean"
        PrimaryType _ -> return . ReturnType $ t
        _ -> stopAnalyzer $ "Wrong expression type: " ++ show t

checkNumericOp :: Expression -> Expression -> AnalyzerError MethodType
checkNumericOp e1 e2 = do
    et1 <- analyzeReturnExpression e1
    et2 <- analyzeReturnExpression e2
    e <- inferType et1 et2
    checkNumericType e

checkEqOp :: Expression -> Expression -> AnalyzerError MethodType
checkEqOp e1 e2 = do
    et1 <- analyzeReturnExpression e1
    et2 <- analyzeReturnExpression e2
    ReturnType <$> inferType et1 et2

checkUnaryOp :: Expression -> AnalyzerError MethodType
checkUnaryOp e = do
    et <- analyzeReturnExpression e
    checkNumericType et

checkLValue :: QualifiedName -> AnalyzerError ()
checkLValue qn = CM.unless (getValueType qn == LValue) $ stopAnalyzer $ show qn ++ " not l-value"

checkIncDecOp :: QualifiedName -> AnalyzerError MethodType
checkIncDecOp qn = do
    checkLValue qn
    t <- analyzeReturnQualifiedName qn
    checkNumericType t

analyzeExpression :: Expression -> AnalyzerError MethodType
analyzeExpression (Assign qn expr) = do
    checkLValue qn
    q <- analyzeReturnQualifiedName qn
    e <- analyzeReturnExpression expr
    castType e q
    return . ReturnType $ q
analyzeExpression (Or e1 e2) = checkBoolOp e1 e2
analyzeExpression (And e1 e2) = checkBoolOp e1 e2
analyzeExpression (Equal e1 e2) = checkEqOp e1 e2
analyzeExpression (Ne e1 e2) = checkEqOp e1 e2
analyzeExpression (Lt e1 e2) = checkNumericOp e1 e2
analyzeExpression (Gt e1 e2) = checkNumericOp e1 e2
analyzeExpression (Le e1 e2) = checkNumericOp e1 e2
analyzeExpression (Ge e1 e2) = checkNumericOp e1 e2
analyzeExpression (Plus e1 e2) = checkNumericOp e1 e2
analyzeExpression (Minus e1 e2) = checkNumericOp e1 e2
analyzeExpression (Mul e1 e2) = checkNumericOp e1 e2
analyzeExpression (Div e1 e2) = checkNumericOp e1 e2
analyzeExpression (Mod e1 e2) = checkNumericOp e1 e2
analyzeExpression (Pos e) = checkUnaryOp e
analyzeExpression (Neg e) = checkUnaryOp e
analyzeExpression (Not e) = do
    et <- analyzeReturnExpression e
    case et of
        PrimaryType TBoolean -> return . ReturnType $ et
        _ -> stopAnalyzer $ "Wrong experssion type: " ++ show et
analyzeExpression (PreInc e) = checkIncDecOp e
analyzeExpression (PreDec e) = checkIncDecOp e
analyzeExpression (PostInc e) = checkIncDecOp e
analyzeExpression (PostDec e) = checkIncDecOp e
analyzeExpression (QN qn) = analyzeQualifiedName qn
analyzeExpression (Literal l) = return . ReturnType . PrimaryType $ literalType l
analyzeExpression Null = return . ReturnType $ NullType

analyzeReturnExpression :: Expression -> AnalyzerError Type
analyzeReturnExpression expr = do
    mt <- analyzeExpression expr
    case mt of
        ReturnType t -> return t
        _ -> stopAnalyzer "Expression does not return anything"

analyzeExpr :: Expression -> Analyzer (Maybe MethodType)
analyzeExpr expr = do
    s <- get
    let res = runIdentity . runErrorT . runWriterT . runStateT (analyzeExpression expr) $ s
    case res of
        Left err -> do
            newError err
            return Nothing
        Right ((t, st), _) -> do
            put st
            return $ Just t

ifOkExpr :: Expression -> (MethodType -> Analyzer ()) -> Analyzer ()
ifOkExpr expr f = do
    mt <- analyzeExpr expr
    Foldable.forM_ mt f

checkBoolExpr :: Expression -> SemanticError -> Analyzer ()
checkBoolExpr expr err = ifOkExpr expr $ \mt -> CM.unless (mt == ReturnType (PrimaryType TBoolean)) $ newError err

instance Analyzable Type where
    analyze t = do
        defined <- isTypeDefined t
        CM.unless defined $ newError $ "Type " ++ show t ++ " undefined"

instance Analyzable MethodType where
    analyze (ReturnType t) = analyze t
    analyze _ = return ()

instance Analyzable Parameter where
    analyze param = do
        analyze . paramType $ param
        addLocalVar (paramType param) (paramName param) True
        return ()

instance Analyzable Variable where
    analyze (Variable t n expr) = do
        analyze t
        i <- case expr of
            Just e -> do
                ifOkExpr e $ \mt -> case mt of
                    ReturnType t' -> CM.unless (t == t') $ newError $ "Type mismatch: " ++ show t ++ " != " ++ show t'
                    _ -> newError "Cannot assign void"
                return True
            Nothing -> return False
        mth <- gets currentMaybeMethod
        CM.when (isJust mth) $ addLocalVar t n i

instance Analyzable If where
    analyze (If cond cons alt) = do
        checkBoolExpr cond $ "Not boolean condition: " ++ show cond
        analyze cons
        analyzeMaybe alt

instance Analyzable While where
    analyze (While cond st) = do
        checkBoolExpr cond $ "Not boolean condition: " ++ show cond
        analyze st

instance Analyzable ForInit where
    analyze (ForInitVD var) = analyze var
    analyze (ForInitEL ls) = do
        CM.mapM_ analyzeExpr ls
        return ()

instance Analyzable For where
    analyze (For i cond inc st) = do
        addScope
        analyze i
        checkBoolExpr cond $ "Not boolean condition: " ++ show cond
        CM.mapM_ analyzeExpr inc
        analyze st
        removeScope

instance Analyzable Statement where
    analyze (SubBlock b) = analyze b
    analyze (IfStatement st) = analyze st
    analyze (WhileStatement st) = analyze st
    analyze (ForStatement st) = analyze st
    analyze (Return expr) = do
        ret <- methodType <$> gets currentMethod
        case expr of
            Nothing -> CM.unless (ret == Void) $ newError "Missing return value"
            Just e -> ifOkExpr e $ \mt -> CM.unless (ret == mt) $ newError $ "Incompatible types: required " ++ show ret ++ ", found " ++ show mt
    analyze Break = return ()
    analyze Continue = return ()
    analyze (ExpressionStatement expr) = void $ analyzeExpr expr

instance Analyzable BlockStatement where
    analyze (BlockVD var) = analyze var
    analyze (Statement s) = analyze s

instance Analyzable Block where
    analyze b = do
        addScope
        CM.mapM_ analyze b
        removeScope

instance Analyzable Method where
    analyze mth = do
        modify $ \s -> s { currentMaybeMethod = Just mth }
        addScope
        analyze . methodType $ mth
        let params = methodParams mth
        CM.mapM_ analyze params
        analyze . methodBlock $ mth
        removeScope
        modify $ \s -> s { currentMaybeMethod = Nothing }

checkIfConstructor :: Method -> Analyzer ()
checkIfConstructor mth = do
    clsName <- gets $ className . currentClass
    CM.when (methodType mth == Constructor && clsName /= methodName mth) $ newError "Invalid method declaration: return type required"

instance Analyzable Class where
    analyze cls = do
        modify $ \s -> s { currentMaybeClass = Just cls }
        CM.mapM_ analyze $ classFields cls
        CM.mapM_ analyze $ classMethods cls
        modify $ \s -> s { currentMaybeClass = Nothing }

instance Analyzable Program where
    analyze p = do
        modify $ \s -> s { maybeProgram = Just p }
        CM.mapM_ analyze p
        modify $ \s -> s { maybeProgram = Nothing }

