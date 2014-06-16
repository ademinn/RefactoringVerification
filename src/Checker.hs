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

newShowLineError :: (Show a, Monad m) => a -> Int -> SemanticError -> CheckerT m ()
newShowLineError a line e = newError $ "on line " ++ show line ++ ", in \"" ++ show a ++ "\": " ++ e

newLineError :: (WithLine a, Show a, Monad m) => a -> SemanticError -> CheckerT m ()
newLineError a e = newShowLineError a (lineNumber a) e

stopAnalyzer :: SemanticError -> CheckerError a
stopAnalyzer = throwError

stopAnalyzerLine :: (WithLine t, Show t) => t -> SemanticError -> CheckerError a
stopAnalyzerLine t e = stopAnalyzer $ "on line " ++ show (lineNumber t) ++ ", in \"" ++ show t ++ "\": " ++ e

checkEither :: (Show t, WithLine t) => t -> Either SemanticError a -> CheckerError a
checkEither t (Left err) = stopAnalyzerLine t err
checkEither _ (Right res) = return res

addLocalVar :: Type -> String -> Bool -> Checker ()
addLocalVar t n i = do
    res <- newLocalVar n $ VarSymbol n t i
    Foldable.forM_ res newError

checkMethod :: QualifiedName -> ObjectType -> String -> [Type] -> (Method -> Bool) -> CheckerError MethodType
checkMethod qn typeName mth prms f = methodType <$> (findMethod typeName mth prms f >>= (checkEither qn))

checkQualifiedName :: QualifiedName -> CheckerError MethodType
checkQualifiedName q@(QName (FieldAccess qn field) line) = do
    t <- checkQualifiedName qn
    case t of
        ReturnType (ObjectType ot) -> checkField ot field
        Constructor -> error "error in checker"
        t' -> stopAnalyzerLine q $ show t' ++ " has no fields"
    where
        checkField typeName fieldName = do
            (t, _) <- findField typeName fieldName >>= checkEither q
            return $ ReturnType t

checkQualifiedName q@(QName (MethodCall qn mth params) line) = do
    t <- checkQualifiedName qn
    paramTypes <- mapM checkReturnExpression params
    case t of
        ReturnType (ObjectType ot) -> checkMethod q ot mth paramTypes (\m -> methodType m /= Constructor)
        Constructor -> error "error in checkr"
        t' -> stopAnalyzerLine q $ show t' ++ " has no methods"
                            
checkQualifiedName (QName (Var var) line) = do
    vs <- lookupLocalVar var
    case vs of
        Just v -> return . ReturnType . vsType $ v
        Nothing -> checkQualifiedName $ QName (FieldAccess (QName This line) var) line

checkQualifiedName q@(QName (New typeName params) line) = do
    paramTypes <- mapM checkReturnExpression params
    void $ checkMethod q typeName typeName paramTypes (\m -> methodType m == Constructor)
    return . ReturnType . ObjectType $ typeName
checkQualifiedName (QName This line) = ReturnType <$> ObjectType <$> classNameM

checkReturnQualifiedName :: QualifiedName -> CheckerError Type
checkReturnQualifiedName qn = do
    q <- checkQualifiedName qn
    case q of
        ReturnType t -> return t
        _ -> stopAnalyzerLine qn "Qualified name has no value"

getValueType :: QualifiedName -> ValueType
getValueType (QName (FieldAccess _ _) line) = LValue
getValueType (QName (Var _) line) = LValue
getValueType _ = RValue

inferType :: Expression -> Type -> Type -> CheckerError Type
inferType e t1 t2 = do
    let mt = infer t1 t2
    case mt of
        Just t -> return t
        Nothing -> stopAnalyzerLine e $ "Incompatible types: " ++ show t1 ++ ", " ++ show t2

castType :: Expression -> Type -> Type -> CheckerError ()
castType e t1 t2 = CM.unless (cast t1 t2) $ stopAnalyzerLine e $ "Can't cast " ++ show t1 ++ " to " ++ show t2

checkBoolOp :: Expression -> Expression -> Expression -> CheckerError MethodType
checkBoolOp e e1 e2 = do
    et1 <- checkReturnExpression e1
    et2 <- checkReturnExpression e2
    case (et1, et2) of
        (PrimaryType TBoolean, PrimaryType TBoolean) -> return . ReturnType . PrimaryType $ TBoolean
        (PrimaryType TBoolean, _) -> stopAnalyzerLine e $ "Not boolean expression: " ++ show e2
        (_, PrimaryType TBoolean) -> stopAnalyzerLine e $ "Not boolean expression: " ++ show e1
        _ -> stopAnalyzerLine e $ "Not boolean expressions: " ++ show e1 ++ ", " ++ show e2

checkNumericType :: Type -> CheckerError MethodType
checkNumericType t = case t of
        PrimaryType TBoolean -> stopAnalyzer "Wrong expression type: boolean"
        PrimaryType _ -> return . ReturnType $ t
        _ -> stopAnalyzer $ "Wrong expression type: " ++ show t

checkNumericOp :: Expression -> Expression -> Expression -> CheckerError MethodType
checkNumericOp ex e1 e2 = do
    et1 <- checkReturnExpression e1
    et2 <- checkReturnExpression e2
    e <- inferType ex et1 et2
    checkNumericType e

checkEqOp :: Expression -> Expression -> Expression -> CheckerError MethodType
checkEqOp e e1 e2 = do
    et1 <- checkReturnExpression e1
    et2 <- checkReturnExpression e2
    ReturnType <$> inferType e et1 et2

checkUnaryOp :: Expression -> CheckerError MethodType
checkUnaryOp e = do
    et <- checkReturnExpression e
    checkNumericType et

checkLValue :: QualifiedName -> CheckerError ()
checkLValue qn = CM.unless (getValueType qn == LValue) $ stopAnalyzerLine qn $ show qn ++ " not l-value"

checkIncDecOp :: QualifiedName -> CheckerError MethodType
checkIncDecOp qn = do
    checkLValue qn
    t <- checkReturnQualifiedName qn
    checkNumericType t

retBoolean :: CheckerError MethodType
retBoolean = return . ReturnType . PrimaryType $ TBoolean

checkExpression :: Expression -> CheckerError MethodType
checkExpression ex@(Expr (Assign qn expr) line) = do
    checkLValue qn
    q <- checkReturnQualifiedName qn
    e <- checkReturnExpression expr
    castType ex e q
    return . ReturnType $ q
checkExpression ex@(Expr (Or e1 e2) line) = checkBoolOp ex e1 e2
checkExpression ex@(Expr (And e1 e2) line) = checkBoolOp ex e1 e2
checkExpression ex@(Expr (Equal e1 e2) line) = checkEqOp ex e1 e2 >> retBoolean
checkExpression ex@(Expr (Ne e1 e2) line) = checkEqOp ex e1 e2 >> retBoolean
checkExpression ex@(Expr (Lt e1 e2) line) = checkNumericOp ex e1 e2 >> retBoolean
checkExpression ex@(Expr (Gt e1 e2) line) = checkNumericOp ex e1 e2 >> retBoolean
checkExpression ex@(Expr (Le e1 e2) line) = checkNumericOp ex e1 e2 >> retBoolean
checkExpression ex@(Expr (Ge e1 e2) line) = checkNumericOp ex e1 e2 >> retBoolean
checkExpression ex@(Expr (Plus e1 e2) line) = checkNumericOp ex e1 e2
checkExpression ex@(Expr (Minus e1 e2) line) = checkNumericOp ex e1 e2
checkExpression ex@(Expr (Mul e1 e2) line) = checkNumericOp ex e1 e2
checkExpression ex@(Expr (Div e1 e2) line) = checkNumericOp ex e1 e2
checkExpression ex@(Expr (Mod e1 e2) line) = checkNumericOp ex e1 e2
checkExpression (Expr (Pos e) line) = checkUnaryOp e
checkExpression (Expr (Neg e) line) = checkUnaryOp e
checkExpression ex@(Expr (Not e) line) = do
    et <- checkReturnExpression e
    case et of
        PrimaryType TBoolean -> return . ReturnType $ et
        _ -> stopAnalyzerLine ex $ "Wrong expression type: " ++ show et
checkExpression (Expr (PreInc e) line) = checkIncDecOp e
checkExpression (Expr (PreDec e) line) = checkIncDecOp e
checkExpression (Expr (PostInc e) line) = checkIncDecOp e
checkExpression (Expr (PostDec e) line) = checkIncDecOp e
checkExpression (Expr (QN qn) line) = checkQualifiedName qn
checkExpression (Expr (Literal l) line) = return . ReturnType . PrimaryType $ literalType l
checkExpression (Expr Null line) = return . ReturnType $ NullType

checkReturnExpression :: Expression -> CheckerError Type
checkReturnExpression expr = do
    mt <- checkExpression expr
    case mt of
        ReturnType t -> return t
        _ -> stopAnalyzerLine expr "Expression does not return anything"

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
checkBoolExpr expr err = ifOkExpr expr $ \mt -> CM.unless (mt == ReturnType (PrimaryType TBoolean)) $ newLineError expr err

checkReturnSt :: Statement -> Checker Bool
checkReturnSt (Return _ _) = return True
checkReturnSt (IfStatement (If _ _ Nothing)) = return False
checkReturnSt (IfStatement (If _ st1 (Just st2))) = (&&) <$> checkReturnSt st1 <*> checkReturnSt st2
checkReturnSt _ = return False

findRedefs :: [Method] -> [Method]
findRedefs mths = fst $ foldl f ([], []) mths
    where
        f l@(redef, al) m =
            let cms = compareMethodSignatures m in
            if any cms al
                then
                    if any cms redef then l else (m:redef, al)
                else
                    (redef, m:al)

compareMethodSignatures :: Method -> Method -> Bool
compareMethodSignatures (Method _ n1 p1 _) (Method _ n2 p2 _) = n1 == n2 && (map paramType p1) == (map paramType p2)

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
    check v@(Variable t n expr) = do
        check t
        i <- case expr of
            Just e -> do
                ifOkExpr e $ \mt -> case mt of
                    ReturnType t' -> CM.unless (cast t' t) $ newShowLineError v (lineNumber e) $ "Type mismatch: " ++ show t ++ " != " ++ show t'
                    _ -> newShowLineError v (lineNumber e) "Cannot assign void"
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
    check r@(Return expr line) = do
        ret <- methodTypeM
        case expr of
            Nothing -> CM.unless (ret == Void) $ newShowLineError r line "Missing return value"
            Just e -> ifOkExpr e $ \mt -> CM.unless (ret == mt) $ newShowLineError r line $ "Incompatible types: required " ++ show ret ++ ", found " ++ show mt
    check (Break line) = return ()
    check (Continue line) = return ()
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
        cls <- getClassM
        modify $ \s -> s { currentMaybeMethod = Just mth }
        addScope
        check . methodType $ mth
        let params = methodParams mth
        CM.mapM_ check params
        check . methodBlock $ mth
        CM.unless (methodType mth == Void || methodType mth == Constructor) $ do
            ret <- if null (methodBlock mth) then return False else checkBlockSt . last . methodBlock $ mth
            CM.unless ret $ newError $ "in " ++ showMethod cls mth ++ ": missing return statement"
        removeScope
        modify $ \s -> s { currentMaybeMethod = Nothing }
        where
            checkBlockSt (Statement s) = checkReturnSt s
            checkBlockSt _ = return False

instance Checkable Class where
    check cls = do
        modify $ \s -> s { currentMaybeClass = Just cls }
        CM.mapM_ check $ classFields cls
        let redefs = findRedefs $ classMethods cls
        CM.forM_ redefs $ newError . (\m -> "method redefinition: " ++ showMethod cls m)
        CM.mapM_ check $ classMethods cls
        modify $ \s -> s { currentMaybeClass = Nothing }

instance Checkable Program where
    check p = do
        modify $ \s -> s { maybeProgram = Just $ nativeClasses ++ p }
        CM.mapM_ check p
        modify $ \s -> s { maybeProgram = Nothing }

