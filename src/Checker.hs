module Checker where

import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map
import Control.Monad.Except
import Control.Monad.State

import qualified ParseTree as PT
import AST

type Checker a = ExceptT String a

check :: Bool -> String -> Checker ()
check True _ = return ()
check False s = throwError s

checkUnique :: (a -> a -> Bool) -> (a -> String) -> [a] -> Checker ()
checkUnique eqFunc showErr ls = mapM checkElem ls
    where
        checkElem e = check (length (filter (eqFunc e) ls) <= 1) $ showErr e

checkVariablesUnique :: [Variable] -> Checker ()
checkVariablesUnique = checkUnique (\(Variable _ n1) (Variable _ n2) -> n1 == n2) (\v -> "Variable " ++ show . varName $ v ++ " already exists")

getClass :: PT.Program -> Type -> Maybe PT.Class
getClass program t = find (\cls -> PT.clsName cls == t) program

checkType :: PT.Program -> Type -> Checker PT.Class
checkType program t = case getClass program t of
    Just c -> return c
    Nothing -> throwError "Class " ++ t ++ " not found"

checkVariableType :: PT.Program -> Variable -> Checker ()
checkVariableType program var = checkType program $ varType var

checkVariablesTypes :: PT.Program -> [Variable] -> Checker ()
checkVariablesTypes program = mapM (checkVariableType program)

getVarType :: [Variable] -> PT.Identifier -> Checker Type
getvarType vars id = if null tVars
    then 
        throwError "no variable " ++ id ++ " found"
    else
        return $ head tVars
    where
        tVars = mapMaybe (\(Variable t n) -> if n == id then Just t else Nothing) vars

checkParameters :: [Variable] -> [Type] -> Checker ()
checkParameters params types = check (map varType params) == types "incompatible types"

getField :: PT.Class -> Identifier -> Checker Variable
getField cls id = case find (\field -> PT.varName field == id) $ PT.clsFields cls of
    Just f -> return f
    Nothing -> throwError "Field " ++ id ++ " not found"

getMethod :: PT.Class -> Identifier -> Checker Method
getMethod cls id = case find (\mth -> PT.mthName mth == id) $ PT.clsMethods cls of
    Just m -> return m
    Nothing -> throwError "Field " ++ id ++ " not found"

checkProgram :: PT.Program -> Checker Program
checkProgram = undefined

checkFields :: PT.Program -> PT.Class -> Checker (Map.Map Identifier Type)
checkFields program (PT.Class _ fields _ _) = do
    checkVariablesTypes program fields
    checkVariablesUnique fields
    return . Map.fromList $ map (\(Variable t n) -> (n, t)) fields

checkConstructor :: PT.Program -> PT.Class -> PT.Constructor -> Checker Constructor
checkConstructor program (PT.Class clsN clsFs _ _) (PT.Constructor consN consPs consB) = do
    check (clsN == consN) "bad constructor name"
    checkVariablesTypes program consPs
    checkVariablesUnique consPs
    checkEqLists (varNames clsFs) consFields
    checkEqLists (varNames consPs) consValues
    return $ Constructor consPs
    where
        checkEqLists l1 l2 = check (Set.fromList l1 == Set.fromList l2) $ show l1 ++ " not equal to " ++ show l2
        varNames = map varName
        consFields = map field consB
        consValues = map value consB

checkMethod :: PT.Program -> PT.Class -> PT.Method -> Checker Method
checkMethod program cls (Method mthT _ mthPs mthE) = do
    checkType program mthT
    checkVariablesTypes program mthPs
    checkVariablesUnique mthPs
    checkExpression mthE
    return $ Method mthPs mthE

checkExpression :: PT.Program -> [Variable] -> Expression -> Checker PT.Class
checkExpression program ctx (New t ps) = do
    newCls <- checkType program t
    exprTypes <- mapM (clsName <$> checkExpression program ctx) ps
    checkParameters  (consParams . head . clsConstructors $ newCls) exprTypes
    return newCls
checkExpression program ctx (FieldAccess expr field) = do
    exprCls <- checkExpression program ctx expr
    f <- getField exprCls field
    checkType program $ PT.varType f
checkExpression program ctx (MethodCall subExpr mthN ps) = do
    subCls <- checkExpression program ctx subExpr
    exprTypes <- mapM (clsName <$> checkExpression program ctx) ps
    mth <- getMethod subCls mthN
    checkParameters (PT.mthParams mth) exprTypes
    checkType program $ PT.mthType mth
checkExpression program ctx (Var id) = do
    t <- getVarType ctx id
    checkType program t
