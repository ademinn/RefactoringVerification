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

checkType :: PT.Program -> Type -> Checker ()
checkType program t = check (isJust . find (\cls -> PT.clsName cls == t) program) "Class " ++ t ++ " not found"

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
    expr <- checkExpression mthE
    return $ Method mthPs expr

checkExpression :: PT.Program -> Expression -> Checker Expression
checkExpression _ = return

