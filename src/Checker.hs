module Checker where

import Data.Maybe
import Data.List
import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map
import Control.Applicative
import Control.Monad.Except

import qualified ParseTree as PT
import AST

type Checker a = Except String a

check :: Bool -> String -> Checker ()
check True _ = return ()
check False s = throwError s

checkUnique :: (a -> a -> Bool) -> (a -> String) -> [a] -> Checker ()
checkUnique eqFunc showErr ls = mapM_ checkElem ls
    where
        checkElem e = check (length (filter (eqFunc e) ls) <= 1) $ showErr e

checkVariablesUnique :: [Variable] -> Checker ()
checkVariablesUnique = checkUnique (\(Variable _ n1) (Variable _ n2) -> n1 == n2) (\v -> "Variable " ++ (show . varName $ v) ++ " already exists")

getClass :: PT.Program -> Type -> Maybe PT.Class
getClass program t = find (\cls -> PT.clsName cls == t) program

checkType :: PT.Program -> Type -> Checker PT.Class
checkType program t = case getClass program t of
    Just c -> return c
    Nothing -> throwError $ "Class " ++ t ++ " not found"

checkVariableType :: PT.Program -> Variable -> Checker ()
checkVariableType program var = void $ checkType program $ varType var

checkVariablesTypes :: PT.Program -> [Variable] -> Checker ()
checkVariablesTypes program = mapM_ (checkVariableType program)

getVarType :: [Variable] -> PT.Identifier -> Checker Type
getVarType vars vName = if null tVars
    then 
        throwError $ "no variable " ++ vName ++ " found"
    else
        return $ head tVars
    where
        tVars = mapMaybe (\(Variable t n) -> if n == vName then Just t else Nothing) vars

checkParameters :: [Variable] -> [Type] -> Checker ()
checkParameters ps types = check (map varType ps == types) "incompatible types"

getField :: PT.Class -> Identifier -> Checker Variable
getField cls fName = case find (\field -> PT.varName field == fName) $ PT.clsFields cls of
    Just f -> return f
    Nothing -> throwError $ "Field " ++ fName ++ " not found"

getMethod :: PT.Class -> Identifier -> Checker PT.Method
getMethod cls mName = case find (\mth -> PT.mthName mth == mName) $ PT.clsMethods cls of
    Just m -> return m
    Nothing -> throwError $ "Field " ++ mName ++ " not found"

checkProgram :: PT.Program -> Checker Program
checkProgram = undefined

checkFields :: PT.Program -> PT.Class -> Checker (Map.Map Identifier Type)
checkFields program (PT.Class _ fs _ _) = do
    checkVariablesTypes program fs
    checkVariablesUnique fs
    return . Map.fromList $ map (\(Variable t n) -> (n, t)) fs

checkConstructor :: PT.Program -> PT.Class -> PT.Constructor -> Checker Constructor
checkConstructor program (PT.Class clsN clsFs _ _) (PT.Constructor consN consPs consB) = do
    check (clsN == consN) "bad constructor name"
    checkVariablesTypes program consPs
    checkVariablesUnique consPs
    checkEqLists (varNames clsFs) consFields
    checkEqLists (varNames consPs) consValues
    return consPs
    where
        checkEqLists l1 l2 = check (Set.fromList l1 == Set.fromList l2) $ show l1 ++ " not equal to " ++ show l2
        varNames = map varName
        consFields = map PT.field consB
        consValues = map PT.value consB

checkMethod :: PT.Program -> PT.Class -> PT.Method -> Checker Method
checkMethod program cls (PT.Method mthT _ mthPs mthE) = do
    void $ checkType program mthT
    checkVariablesTypes program mthPs
    checkVariablesUnique mthPs
    let ctx = Variable (PT.clsName cls) "this" : mthPs
    void $ checkExpression program ctx mthE
    return $ Method mthPs mthE

checkExpression :: PT.Program -> [Variable] -> Expression -> Checker PT.Class
checkExpression program ctx (PT.New t ps) = do
    newCls <- checkType program t
    exprTypes <- mapM (\e -> PT.clsName <$> checkExpression program ctx e) ps
    checkParameters  (PT.consParams . head . PT.clsConstructors $ newCls) exprTypes
    return newCls
checkExpression program ctx (PT.FieldAccess expr field) = do
    exprCls <- checkExpression program ctx expr
    f <- getField exprCls field
    checkType program $ PT.varType f
checkExpression program ctx (PT.MethodCall subExpr mthN ps) = do
    subCls <- checkExpression program ctx subExpr
    exprTypes <- mapM (\e -> PT.clsName <$> checkExpression program ctx e) ps
    mth <- getMethod subCls mthN
    checkParameters (PT.mthParams mth) exprTypes
    checkType program $ PT.mthType mth
checkExpression program ctx (PT.Var vName) = do
    t <- getVarType ctx vName
    checkType program t
