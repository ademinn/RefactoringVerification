module Reduction where

import Data.Maybe
import Data.Map (Map, (!), lookup)

import FJ.AST

type Context = Map Identifier Type

getExpressionClass :: TypedExpression -> Program -> Class
getExpressionClass expr program = program ! valueType expr

lookupMethod :: Program -> Class -> Identifier -> Maybe Method
lookupMethod program cls mthN = case Data.Map.lookup mthN (methods cls) of
    Just m -> Just m
    Nothing -> if base cls == "Object" then Nothing else lookupMethod program (program ! base cls) mthN

getMethod :: Program -> Class -> Identifier -> Method
getMethod program cls mthN = fromJust $ lookupMethod program cls mthN

replaceVariable :: TypedExpression -> Identifier -> TypedExpression -> TypedExpression
replaceVariable (Typed t expr) var rExpr =
    Typed t $ case expr of
        New _ ps -> New t $ map replace ps
        FieldAccess e fN -> FieldAccess (replace e) fN
        MethodCall e mthN ps -> MethodCall (replace e) mthN $ map replace ps
        Var varN -> if varN == var then value rExpr else Var varN
    where
        replace e = replaceVariable e var rExpr

reduceOuterMethodCall :: TypedExpression -> Program -> TypedExpression
reduceOuterMethodCall (Typed _ (MethodCall e mthN ps)) program = foldl foldF mBody foldList
    where
        eCls = getExpressionClass e program
        mth = getMethod program eCls mthN
        mBody = body mth
        foldList = zip (map varName $ params mth) $ e : ps
        foldF expr (paramN, paramV) = replaceVariable expr paramN paramV
reduceOuterMethodCall _ _ = undefined

reduceMethodCall :: TypedExpression -> Identifier -> Identifier -> Program -> TypedExpression
reduceMethodCall (Typed t expr) rType rMthN program =
    Typed t $ case expr of
        New _ ps -> New t $ map reduce ps
        FieldAccess e fN -> FieldAccess (reduce e) fN
        MethodCall e mthN ps -> value $ if eType == rType && mthN == rMthN then reduceOuterMethodCall mth' program else mth'
            where
                eType = valueType e
                e' = reduce e
                ps' = map reduce ps
                mth' = Typed t $ MethodCall e' mthN ps'
        Var varN -> Var varN
    where
        reduce e = reduceMethodCall e rType rMthN program

checkScope :: Program -> Program -> Identifier -> Identifier -> Maybe (TypedExpression, TypedExpression)
checkScope originalProgram modifiedProgram toClassN toMethodN = case lookupMethod originalProgram toClassOrig toMethodN of
        Nothing -> Nothing
        Just m -> let
            expected = body m
            actual = body $ methods toClassMod ! toMethodN
            in if actual == expected then Nothing else Just (actual, expected)
    where
        toClassOrig = originalProgram ! toClassN
        toClassMod = modifiedProgram ! toClassN

checkMethodExtraction :: Program -> Program -> Identifier -> Identifier -> Identifier -> Identifier -> Maybe (TypedExpression, TypedExpression)
checkMethodExtraction originalProgram modifiedProgram fromClassN fromMethodN toClassN toMethodN =
    if actual == expected then
        checkScope originalProgram modifiedProgram toClassN toMethodN
    else
        Just (actual, expected)
    where
        fromClassOrig = originalProgram ! fromClassN
        fromClassMod = modifiedProgram ! fromClassN
        expected = body $ methods fromClassOrig ! fromMethodN
        fromMethod = methods fromClassMod ! fromMethodN
        actual = reduceMethodCall (body fromMethod) toClassN toMethodN modifiedProgram
