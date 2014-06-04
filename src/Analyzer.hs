module Analyzer where

-- import Control.Applicative
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
    , currentClass :: Class
    } deriving Show

type AnalyzerT m a = StateT AnalyzerState (WriterT [SemanticError] m) a

type Analyzer a = AnalyzerT Identity a

type AnalyzerError a = AnalyzerT (ErrorT String Identity) a

class Analyzable a where
    analyze :: a -> Analyzer ()

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

isTypeDefined :: Type -> Analyzer Bool
isTypeDefined (ObjectType typeName) = do
    classNames <- gets $ \s -> map className $ program s
    return $ typeName `elem` classNames
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

lookupLocalVar :: String -> Analyzer (Maybe VarSymbol)
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

analyzeQualifiedName :: QualifiedName -> AnalyzerT Maybe MethodType
analyzeQualifiedName _ = return $ ReturnType $ PrimaryType TBoolean

-- compatiblePrimaryType :: PrimaryType -> PrimaryType -> AnalyzerError PrimaryType
-- compatiblePrimaryType TBoolean TBoolean = return TBoolean
-- compatiblePrimaryType TBoolean _ = stopAnalyzer "Can't choose types"
-- compatiblePrimaryType _ TBoolean = stopAnalyzer "Can't cast types"
-- compatiblePrimaryType t1 t2 = case compare t1 t2 of
--     GT -> return t1
--     EQ -> return t1
--     LT -> return t2
-- 
-- chooseType :: Type -> Type -> AnalyzerError Type
-- chooseType (PrimaryType t1) (PrimaryType t2) = PrimaryType <$> choosePrimaryType t1 t2
-- chooseType _ _ = stopAnalyzer "Can't cast types"

castPrimaryType :: PrimaryType -> PrimaryType -> AnalyzerError ()
castPrimaryType TBoolean TBoolean = return ()
castPrimaryType TBoolean t = stopAnalyzer $ "Can't cast boolean to " ++ show t
castPrimaryType t TBoolean = stopAnalyzer $ "Can't cast " ++ show t ++ " to boolean"
castPrimaryType t1 t2 = CM.unless (t1 <= t2) $ stopAnalyzer $ "Can't cast " ++ show t1 ++ " to " ++ show t2

castType :: Type -> Type -> AnalyzerError ()
castType (PrimaryType t1) (PrimaryType t2) = castPrimaryType t1 t2
castType (PrimaryType t1) t2 = stopAnalyzer $ "Can't cast " ++ show t1 ++ " to " ++ show t2
castType t1 (PrimaryType t2) = stopAnalyzer $ "Can't cast " ++ show t1 ++ " to " ++ show t2
castType _ NullType = error "error in compiler"
castType NullType _ = return ()
castType t1 t2 = CM.unless (t1 == t2) $ stopAnalyzer $ "Can't cast " ++ show t1 ++ " to " ++ show t2

-- assignablePrimaryTypes :: PrimaryType -> PrimaryType -> AnalyzerError PrimaryType

--castTypes (PrimaryType TByte) (PrimaryType TShort) = return $ PrimaryType
--castTypes (PrimaryType TShort) (PrimaryType 
--castTypes (PrimaryType TInt) (PrimaryType 
--castTypes (PrimaryType TLong) (PrimaryType 
--castTypes (PrimaryType TFloat) (PrimaryType 
--castTypes (PrimaryType TDouble) (PrimaryType 

-- analyzeExpression :: Expression -> AnalyzerT Maybe Type
-- analyzeExpression (Assign qn expr) = 
-- analyzeExpression Or Expression Expression
-- analyzeExpression And Expression Expression
-- analyzeExpression Equal Expression Expression
-- analyzeExpression Ne Expression Expression
-- analyzeExpression Lt Expression Expression
-- analyzeExpression Gt Expression Expression
-- analyzeExpression Le Expression Expression
-- analyzeExpression Ge Expression Expression
-- analyzeExpression Plus Expression Expression
-- analyzeExpression Minus Expression Expression
-- analyzeExpression Mul Expression Expression
-- analyzeExpression Div Expression Expression
-- analyzeExpression Mod Expression Expression
-- analyzeExpression Pos Expression
-- analyzeExpression Neg Expression
-- analyzeExpression Not Expression
-- analyzeExpression PreInc QualifiedName
-- analyzeExpression PreDec QualifiedName
-- analyzeExpression PostInc QualifiedName
-- analyzeExpression PostDec QualifiedName
-- analyzeExpression QN QualifiedName
-- analyzeExpression Literal Literal
-- analyzeExpression Null = undefined

instance Analyzable Type where
    analyze t = do
        defined <- isTypeDefined t
        CM.unless defined $ newError $ "Type " ++ show t ++ " undefined"

instance Analyzable MethodType where
    analyze (ReturnType t) = analyze t
    analyze _ = return ()

instance Analyzable Expression where
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
                analyze e
                return True -- FIXME add typecheck
            Nothing -> return False
        addLocalVar t n i

instance Analyzable If where
    analyze (If cond cons alt) = do
        analyze cond
        analyze cons
        analyzeMaybe alt

instance Analyzable While where
    analyze (While cond st) = do
        analyze cond
        analyze st

instance Analyzable ForInit where
    analyze (ForInitVD var) = analyze var
    analyze (ForInitEL ls) = CM.mapM_ analyze ls

instance Analyzable For where
    analyze (For i cond inc st) = do
        addScope
        analyze i
        analyze cond
        CM.mapM_ analyze inc
        analyze st
        removeScope

instance Analyzable Statement where
    analyze (SubBlock b) = analyze b
    analyze (IfStatement st) = analyze st
    analyze (WhileStatement st) = analyze st
    analyze (ForStatement st) = analyze st
    analyze (Return expr) = analyzeMaybe expr
    analyze Break = return ()
    analyze Continue = return ()
    analyze (ExpressionStatement expr) = analyze expr

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
        cls <- gets currentClass
        analyze . methodType $ mth
        let params = Parameter (ObjectType $ className cls) "this" : methodParams mth
        CM.mapM_ analyze params
        analyze . methodBlock $ mth
