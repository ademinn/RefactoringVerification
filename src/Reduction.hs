module Reduction where

import Data.Map (Map, (!))

import FJ.AST

type Context = Map Identifier Type

getExpressionClass :: TypedExpression -> Program -> Class
getExpressionClass expr program = program ! valueType expr

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
        mth = methods eCls ! mthN
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

checkMethodExtraction :: Program -> Program -> Identifier -> Identifier -> Identifier -> Identifier -> Maybe (TypedExpression, TypedExpression)
checkMethodExtraction originalProgram modifiedProgram fromClassN fromMethodN toClassN toMethodN = if actual == expected then Nothing else Just (actual, expected)
    where
        expected = body $ methods (originalProgram ! fromClassN) ! fromMethodN
        fromMethod = methods (modifiedProgram ! fromClassN) ! fromMethodN
        actual = reduceMethodCall (body fromMethod) toClassN toMethodN modifiedProgram
