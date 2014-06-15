module Checker where

import Control.Applicative
import qualified Control.Monad as CM
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Monad.Error
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Foldable as Foldable

import Scope
import AST
import Type
import Analyzer
import Native

type FieldsIndexes = Map.Map String (Map.Map String Int)

data VarSymbol
    = VarSymbol
    { vsName :: String
    , vsType :: Type
    , vsInit :: Bool
    } deriving Show

type SemanticError = String

data CheckerState
    = CheckerState
    { maybeProgram :: Maybe Program
    , stScope :: Scope VarSymbol
    , currentMaybeClass :: Maybe Class
    , currentMaybeMethod :: Maybe Method
    } deriving Show

type CheckerT m a = StateT CheckerState (WriterT [SemanticError] m) a

type Checker a = CheckerT Identity a

type CheckerError a = CheckerT (ErrorT String Identity) a

data ValueType = LValue | RValue
    deriving (Show, Eq, Ord)

class Checkable a where
    check :: a -> Checker ()

instance Scoped CheckerState VarSymbol where
    getScope = stScope
    setScope st sc = st { stScope = sc }

instance WithProgram CheckerState where
    getProgram = fromJust . maybeProgram

instance WithClass CheckerState where
    getClass = fromJust . currentMaybeClass

instance WithMethod CheckerState where
    getMethod = fromJust . currentMaybeMethod

defaultAnalyzerState :: CheckerState
defaultAnalyzerState = CheckerState Nothing [] Nothing Nothing

checkMaybe :: Checkable a => Maybe a -> Checker ()
checkMaybe = Foldable.mapM_ check 

newError :: Monad m => SemanticError -> CheckerT m ()
newError err = tell [err]

stopAnalyzer :: SemanticError -> CheckerError a
stopAnalyzer = throwError

checkEither :: Either SemanticError a -> CheckerError a
checkEither (Left err) = stopAnalyzer err
checkEither (Right res) = return res

addLocalVar :: Type -> String -> Bool -> Checker ()
addLocalVar t n i = do
    res <- newLocalVar n $ VarSymbol n t i
    Foldable.forM_ res newError

checkClass :: String -> CheckerError Class
checkClass clsName = findClass clsName >>= checkEither

checkMethod :: ObjectType -> String -> [Type] -> (Method -> Bool) -> CheckerError MethodType
checkMethod typeName mth prms f = methodType <$> (findMethod typeName mth prms f >>= checkEither)

checkQualifiedName :: QualifiedName -> CheckerError MethodType
checkQualifiedName (FieldAccess qn field) = do
    t <- checkQualifiedName qn
    case t of
        ReturnType (ObjectType ot) -> checkField ot field
        Constructor -> error "error in checkr"
        t' -> stopAnalyzer $ show t' ++ " has no fields"
    where
        checkField typeName fieldName = do
            (t, _) <- findField typeName fieldName >>= checkEither
            return $ ReturnType t

checkQualifiedName (MethodCall qn mth params) = do
    t <- checkQualifiedName qn
    paramTypes <- mapM checkReturnExpression params
    case t of
        ReturnType (ObjectType ot) -> checkMethod ot mth paramTypes (\m -> methodType m /= Constructor)
        Constructor -> error "error in checkr"
        t' -> stopAnalyzer $ show t' ++ " has no methods"
                            
checkQualifiedName (Var var) = do
    vs <- lookupLocalVar var
    case vs of
        Just v -> return . ReturnType . vsType $ v
        Nothing -> checkQualifiedName $ FieldAccess This var

checkQualifiedName (New typeName params) = do
    paramTypes <- mapM checkReturnExpression params
    void $ checkMethod typeName typeName paramTypes (\m -> methodType m == Constructor)
    return . ReturnType . ObjectType $ typeName
checkQualifiedName This = ReturnType <$> ObjectType <$> classNameM

checkReturnQualifiedName :: QualifiedName -> CheckerError Type
checkReturnQualifiedName qn = do
    q <- checkQualifiedName qn
    case q of
        ReturnType t -> return t
        _ -> stopAnalyzer "Qualified name has no value"

getValueType :: QualifiedName -> ValueType
getValueType (FieldAccess _ _) = LValue
getValueType (Var _) = LValue
getValueType _ = RValue

inferType :: Type -> Type -> CheckerError Type
inferType t1 t2 = do
    let mt = infer t1 t2
    case mt of
        Just t -> return t
        Nothing -> stopAnalyzer $ "Can not infer types: " ++ show t1 ++ " " ++ show t2

castType :: Type -> Type -> CheckerError ()
castType t1 t2 = CM.unless (cast t1 t2) $ stopAnalyzer $ "Can't cast " ++ show t1 ++ " to " ++ show t2

checkBoolOp :: Expression -> Expression -> CheckerError MethodType
checkBoolOp e1 e2 = do
    et1 <- checkReturnExpression e1
    et2 <- checkReturnExpression e2
    case (et1, et2) of
        (PrimaryType TBoolean, PrimaryType TBoolean) -> return . ReturnType . PrimaryType $ TBoolean
        (PrimaryType TBoolean, _) -> stopAnalyzer $ "Not boolean expression: " ++ show e2
        (_, PrimaryType TBoolean) -> stopAnalyzer $ "Not boolean expression: " ++ show e1
        _ -> stopAnalyzer $ "Not boolean expressions: " ++ show e1 ++ ", " ++ show e2

checkNumericType :: Type -> CheckerError MethodType
checkNumericType t = case t of
        PrimaryType TBoolean -> stopAnalyzer "Wrong expression type: boolean"
        PrimaryType _ -> return . ReturnType $ t
        _ -> stopAnalyzer $ "Wrong expression type: " ++ show t

checkNumericOp :: Expression -> Expression -> CheckerError MethodType
checkNumericOp e1 e2 = do
    et1 <- checkReturnExpression e1
    et2 <- checkReturnExpression e2
    e <- inferType et1 et2
    checkNumericType e

checkEqOp :: Expression -> Expression -> CheckerError MethodType
checkEqOp e1 e2 = do
    et1 <- checkReturnExpression e1
    et2 <- checkReturnExpression e2
    ReturnType <$> inferType et1 et2

checkUnaryOp :: Expression -> CheckerError MethodType
checkUnaryOp e = do
    et <- checkReturnExpression e
    checkNumericType et

checkLValue :: QualifiedName -> CheckerError ()
checkLValue qn = CM.unless (getValueType qn == LValue) $ stopAnalyzer $ show qn ++ " not l-value"

checkIncDecOp :: QualifiedName -> CheckerError MethodType
checkIncDecOp qn = do
    checkLValue qn
    t <- checkReturnQualifiedName qn
    checkNumericType t

retBoolean :: CheckerError MethodType
retBoolean = return . ReturnType . PrimaryType $ TBoolean

checkExpression :: Expression -> CheckerError MethodType
checkExpression (Assign qn expr) = do
    checkLValue qn
    q <- checkReturnQualifiedName qn
    e <- checkReturnExpression expr
    castType e q
    return . ReturnType $ q
checkExpression (Or e1 e2) = checkBoolOp e1 e2
checkExpression (And e1 e2) = checkBoolOp e1 e2
checkExpression (Equal e1 e2) = checkEqOp e1 e2 >> retBoolean
checkExpression (Ne e1 e2) = checkEqOp e1 e2 >> retBoolean
checkExpression (Lt e1 e2) = checkNumericOp e1 e2 >> retBoolean
checkExpression (Gt e1 e2) = checkNumericOp e1 e2 >> retBoolean
checkExpression (Le e1 e2) = checkNumericOp e1 e2 >> retBoolean
checkExpression (Ge e1 e2) = checkNumericOp e1 e2 >> retBoolean
checkExpression (Plus e1 e2) = checkNumericOp e1 e2
checkExpression (Minus e1 e2) = checkNumericOp e1 e2
checkExpression (Mul e1 e2) = checkNumericOp e1 e2
checkExpression (Div e1 e2) = checkNumericOp e1 e2
checkExpression (Mod e1 e2) = checkNumericOp e1 e2
checkExpression (Pos e) = checkUnaryOp e
checkExpression (Neg e) = checkUnaryOp e
checkExpression (Not e) = do
    et <- checkReturnExpression e
    case et of
        PrimaryType TBoolean -> return . ReturnType $ et
        _ -> stopAnalyzer $ "Wrong experssion type: " ++ show et
checkExpression (PreInc e) = checkIncDecOp e
checkExpression (PreDec e) = checkIncDecOp e
checkExpression (PostInc e) = checkIncDecOp e
checkExpression (PostDec e) = checkIncDecOp e
checkExpression (QN qn) = checkQualifiedName qn
checkExpression (Literal l) = return . ReturnType . PrimaryType $ literalType l
checkExpression Null = return . ReturnType $ NullType

checkReturnExpression :: Expression -> CheckerError Type
checkReturnExpression expr = do
    mt <- checkExpression expr
    case mt of
        ReturnType t -> return t
        _ -> stopAnalyzer "Expression does not return anything"

checkExpr :: Expression -> Checker (Maybe MethodType)
checkExpr expr = do
    s <- get
    let res = runIdentity . runErrorT . runWriterT . runStateT (checkExpression expr) $ s
    case res of
        Left err -> do
            newError err
            return Nothing
        Right ((t, st), _) -> do
            put st
            return $ Just t

ifOkExpr :: Expression -> (MethodType -> Checker ()) -> Checker ()
ifOkExpr expr f = do
    mt <- checkExpr expr
    Foldable.forM_ mt f

checkBoolExpr :: Expression -> SemanticError -> Checker ()
checkBoolExpr expr err = ifOkExpr expr $ \mt -> CM.unless (mt == ReturnType (PrimaryType TBoolean)) $ newError err

instance Checkable Type where
    check t = do
        defined <- isTypeDefined t
        CM.unless defined $ newError $ "Type " ++ show t ++ " undefined"

instance Checkable MethodType where
    check (ReturnType t) = check t
    check _ = return ()

instance Checkable Parameter where
    check param = do
        check . paramType $ param
        addLocalVar (paramType param) (paramName param) True
        return ()

instance Checkable Variable where
    check (Variable t n expr) = do
        check t
        i <- case expr of
            Just e -> do
                ifOkExpr e $ \mt -> case mt of
                    ReturnType t' -> CM.unless (cast t' t) $ newError $ "Type mismatch: " ++ show t ++ " != " ++ show t'
                    _ -> newError "Cannot assign void"
                return True
            Nothing -> return False
        mth <- gets currentMaybeMethod
        CM.when (isJust mth) $ addLocalVar t n i

instance Checkable If where
    check (If cond cons alt) = do
        checkBoolExpr cond $ "Not boolean condition: " ++ show cond
        check cons
        checkMaybe alt

instance Checkable While where
    check (While cond st) = do
        checkBoolExpr cond $ "Not boolean condition: " ++ show cond
        check st

instance Checkable ForInit where
    check (ForInitVD var) = check var
    check (ForInitEL ls) = do
        CM.mapM_ checkExpr ls
        return ()

instance Checkable For where
    check (For i cond inc st) = do
        addScope
        check i
        checkBoolExpr cond $ "Not boolean condition: " ++ show cond
        CM.mapM_ checkExpr inc
        check st
        removeScope

instance Checkable Statement where
    check (SubBlock b) = check b
    check (IfStatement st) = check st
    check (WhileStatement st) = check st
    check (ForStatement st) = check st
    check (Return expr) = do
        ret <- methodTypeM
        case expr of
            Nothing -> CM.unless (ret == Void) $ newError "Missing return value"
            Just e -> ifOkExpr e $ \mt -> CM.unless (ret == mt) $ newError $ "Incompatible types: required " ++ show ret ++ ", found " ++ show mt
    check Break = return ()
    check Continue = return ()
    check (ExpressionStatement expr) = void $ checkExpr expr

instance Checkable BlockStatement where
    check (BlockVD var) = check var
    check (Statement s) = check s

instance Checkable Block where
    check b = do
        addScope
        CM.mapM_ check b
        removeScope

instance Checkable Method where
    check mth = do
        modify $ \s -> s { currentMaybeMethod = Just mth }
        addScope
        check . methodType $ mth
        let params = methodParams mth
        CM.mapM_ check params
        check . methodBlock $ mth
        removeScope
        modify $ \s -> s { currentMaybeMethod = Nothing }

checkIfConstructor :: Method -> Checker ()
checkIfConstructor mth = do
    clsName <- classNameM
    CM.when (methodType mth == Constructor && clsName /= methodName mth) $ newError "Invalid method declaration: return type required"

instance Checkable Class where
    check cls = do
        modify $ \s -> s { currentMaybeClass = Just cls }
        CM.mapM_ check $ classFields cls
        CM.mapM_ check $ classMethods cls
        modify $ \s -> s { currentMaybeClass = Nothing }

instance Checkable Program where
    check p = do
        modify $ \s -> s { maybeProgram = Just $ nativeClasses ++ p }
        CM.mapM_ check p
        modify $ \s -> s { maybeProgram = Nothing }

