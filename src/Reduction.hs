module Checker where

import Data.Map (Map, (!))

import AST

type Context = Map Identifier Type

getExpressionType :: Expression -> Program -> Context -> Type
getExpressionType (New t _) _ _ = t
getExpressionType (FieldAccess e fN) program ctx = fields (getExpressionClass e program ctx) ! fN
getExpressionType (MethodCall e mthN _) program ctx = mthType $ methods (getExpressionClass e program ctx) ! mthN
getExpressionType (Var varN) _ ctx = ctx ! varN

getExpressionClass :: Expression -> Program -> Context -> Class
getExpressionClass expr program ctx = program ! getExpressionType expr program ctx

replaceVariable :: Expression -> Identifier -> Expression -> Expression
replaceVariable expr var rExpr =
    case expr of
        New t ps -> New t $ map replace ps
        FieldAccess e fN -> FieldAccess (replace e) fN
        MethodCall e mthN ps -> MethodCall (replace e) mthN $ map replace ps
        Var varN -> if varN == var then rExpr else Var varN
    where
        replace e = replaceVariable e var rExpr

reduceOuterMethodCall :: Expression -> Program -> Context -> Expression
reduceOuterMethodCall (MethodCall e mthN ps) program ctx = foldl foldF mBody foldList
    where
        eCls = getExpressionClass e program ctx
        mth = methods eCls ! mthN
        mBody = body mth
        foldList = zip (map varName $ params mth) $ e : ps
        foldF expr (paramN, paramV) = replaceVariable expr paramN paramV
reduceOuterMethodCall _ _ _ = undefined

reduceMethodCall :: Expression -> Identifier -> Program -> Context -> Expression
reduceMethodCall expr rMthN program ctx =
    case expr of
        New t ps -> New t $ map reduce ps
        FieldAccess e fN -> FieldAccess (reduce e) fN
        MethodCall e mthN ps -> if mthN == rMthN then reduce mth' else mth'
            where
                e' = reduce e
                ps' = map reduce ps
                mth' = MethodCall e' mthN ps'
        Var varN -> Var varN
    where
        reduce e = reduceMethodCall e rMthN program ctx
