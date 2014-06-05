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

import AST

type FieldsIndexes = Map.Map String (Map.Map String Int)

data VarSymbol
    = VarSymbol
    { vsName :: String
    , vsType :: Type
    , vsInit :: Bool
    } deriving Show

type BlockVariables = Map.Map String VarSymbol

type Scope = [BlockVariables]

data SymbolTable
    = SymbolTable
    { fieldsIndexes :: FieldsIndexes
    , scope :: Scope
    } deriving Show

type SemanticError = String

data AnalyzerState
    = AnalyzerState
    { program :: Program
    , symbolTable :: SymbolTable
    , currentMaybeClass :: Maybe Class
    , currentMaybeMethod :: Maybe Method
    } deriving Show

type AnalyzerT m a = StateT AnalyzerState (WriterT [SemanticError] m) a

type Analyzer a = AnalyzerT Identity a

type AnalyzerError a = AnalyzerT (ErrorT String Identity) a

data ValueType = LValue | RValue
    deriving (Show, Eq, Ord)

data Ternary = Zero | Half | One
    deriving (Show, Eq, Ord)

class Analyzable a where
    analyze :: a -> Analyzer ()

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

updateScope :: (Scope -> Scope) -> Analyzer ()
updateScope f = do
    st <- get
    let sym = symbolTable st
    let ls = scope sym
    put st { symbolTable = sym { scope = f ls } }

addScope :: Analyzer ()
addScope = updateScope $ \s -> Map.empty : s

removeScope :: Analyzer ()
removeScope = updateScope tail

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

findLocalVar :: String -> Scope -> Maybe VarSymbol
findLocalVar n l = join . List.find isJust $ map (Map.lookup n) l

lookupLocalVar :: (Monad m) => String -> AnalyzerT m (Maybe VarSymbol)
lookupLocalVar n = do
    vars <- gets $ scope . symbolTable
    return $ findLocalVar n vars

isLocalVarDefined :: String -> Analyzer Bool
isLocalVarDefined n = do
    var <- lookupLocalVar n
    return $ isJust var

getLocalVar :: String -> Analyzer (Maybe VarSymbol)
getLocalVar n = do
    var <- lookupLocalVar n
    CM.unless (isJust var) $ newError $ "Variable " ++ n ++ " not defined"
    return var

addLocalVar :: Type -> String -> Bool -> Analyzer ()
addLocalVar t n i = do
    defined <- isLocalVarDefined n
    if defined
        then newError $ "Variable with name " ++ n ++ " already defined"
        else updateScope $ \(l:ls) -> Map.insert n (VarSymbol n t i) l : ls

checkClass :: String -> AnalyzerError Class
checkClass clsName = do
    mcls <- findType clsName
    case mcls of
        Just cls -> return cls
        Nothing -> stopAnalyzer $ "No class with name " ++ clsName

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
castTypeLists from to = List.minimum $ List.zipWith canCast from to

filterMethod :: Method -> [Type] -> Ternary
filterMethod ms params = castTypeLists params $ map paramType $ methodParams ms

checkMethod :: ObjectType -> String -> [Type] -> AnalyzerError MethodType
checkMethod typeName mth prms = do
    cls <- checkClass typeName
    let methods = filter (\m -> (methodName m == mth) && (filterMethod m prms /= Zero)) $ classMethods cls
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
        ReturnType (ObjectType ot) -> checkMethod ot method paramTypes
        Constructor -> error "error in analyzer"
        t' -> stopAnalyzer $ show t' ++ " has no methods"
                            
analyzeQualifiedName (Var var) = do
    vs <- lookupLocalVar var
    case vs of
        Just v -> return . ReturnType . vsType $ v
        Nothing -> analyzeQualifiedName $ FieldAccess This var

analyzeQualifiedName (New typeName params) = do
    paramTypes <- mapM analyzeReturnExpression params
    _ <- checkMethod typeName typeName paramTypes
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
        addLocalVar t n i

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
        analyze . methodType $ mth
        let params = methodParams mth
        CM.mapM_ analyze params
        analyze . methodBlock $ mth
