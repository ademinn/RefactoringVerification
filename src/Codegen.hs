module Codegen where

import qualified LLVM.General.AST as A
import qualified LLVM.General.AST.Type as T
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Instruction as I
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.IntegerPredicate as IP
import qualified LLVM.General.AST.FloatingPointPredicate as FPP

import Control.Applicative
import Control.Monad.State

import Data.Maybe
import Data.Word
import qualified Data.List as List
import qualified Data.Map as Map

import Scope
import AST
import Analyzer
import Type

data CodegenState
    = CodegenState
    { csProgram :: Maybe Program
    , csScope :: Scope (Type, A.Operand)
    , csClass :: Maybe Class
    , csMethod :: Maybe Method
    , lastInd :: Word
    , instructions :: [A.Named I.Instruction]
    , structSizes :: Map.Map ObjectType A.Operand
    , lastLabel :: Word
    }

instance Scoped CodegenState (Type, A.Operand) where
    getScope = csScope
    setScope cs s = cs { csScope = s }

instance WithProgram CodegenState where
    getProgram = fromJust . csProgram

instance WithClass CodegenState where
    getClass = fromJust . csClass

instance WithMethod CodegenState where
    getMethod = fromJust . csMethod

type Codegen a = State CodegenState a

mapPrimaryType :: PrimaryType -> T.Type
mapPrimaryType TBoolean = T.IntegerType 1
mapPrimaryType TByte = T.IntegerType 8
mapPrimaryType TShort = T.IntegerType 16
mapPrimaryType TInt = T.IntegerType 32
mapPrimaryType TLong = T.IntegerType 64
mapPrimaryType TFloat = T.FloatingPointType 32 T.IEEE
mapPrimaryType TDouble = T.FloatingPointType 64 T.IEEE

literalToOp :: Literal -> A.Operand
literalToOp l = A.ConstantOperand $ f l
    where
        f (LBoolean False) = C.Int 1 0
        f (LBoolean True) = C.Int 1 1
        f (LInt i) = C.Int 32 $ toInteger i
        f (LLong i) = C.Int 64 $ toInteger i
        f (LFloat i) = C.Float $ F.Single i
        f (LDouble i) = C.Float $ F.Double i

mapType :: Type -> T.Type
mapType (PrimaryType t) = mapPrimaryType t
mapType (ObjectType t) = T.NamedTypeReference (A.Name t) 
mapType NullType = error "mapType"

mapMethodType :: Class -> MethodType -> T.Type
mapMethodType _ (ReturnType t) = mapType t
mapMethodType _ Void = T.VoidType
mapMethodType cls Constructor = A.NamedTypeReference . A.Name . className $ cls

nextName :: Codegen A.Name
nextName = do
    i <- gets lastInd
    let i' = i + 1
    modify $ \s -> s { lastInd = i' }
    return $ A.UnName i'

addInstr :: I.Instruction -> Codegen A.Operand
addInstr instr = do
    n <- nextName
    modify $ \s -> s { instructions = instructions s ++ [n A.:= instr] }
    return $ A.LocalReference n

addVoidInstr :: I.Instruction -> Codegen ()
addVoidInstr instr = modify $ \s -> s { instructions = instructions s ++ [A.Do instr] }

fromRight :: Either a b -> b
fromRight (Right a) = a
fromRight (Left _) = error "fromRight"

objType :: Type -> ObjectType
objType (ObjectType t) = t
objType _ = error "objType"

structPtrSize :: Word32
structPtrSize = 32

alignment :: Word32
alignment = 0

structFieldAddr :: Int -> A.Operand
structFieldAddr i = A.ConstantOperand . C.Int structPtrSize $ toInteger i

castPrimatyTypeOperand :: (PrimaryType, A.Operand) -> PrimaryType -> Codegen A.Operand
castPrimatyTypeOperand (ot, op) t = if ot == t then return op else addInstr $ f t op (mapPrimaryType t) []
    where
        f TInt = I.SExt
        f TLong = I.SExt
        f TFloat = I.FPExt
        f TDouble = I.FPExt
        f _ = error "cast primary type operand"

castOperand :: (Type, A.Operand) -> Type -> Codegen A.Operand
castOperand (NullType, _) (ObjectType t) = return . A.ConstantOperand $ C.Null $ mapType $ ObjectType t
castOperand (PrimaryType pt, op) (PrimaryType t) = castPrimatyTypeOperand (pt, op) t
castOperand (ObjectType ot, op) (ObjectType t) = if ot == t then return op else error "cast operand"
castOperand _ _ = error "cast operand"

castList :: [(Type, A.Operand)] -> [Type] -> Codegen [A.Operand]
castList ops ts = forM (zip ops ts) (uncurry castOperand)

inferOperand :: (Type, A.Operand) -> (Type, A.Operand) -> Codegen (Type, (A.Operand, A.Operand))
inferOperand (t1, o1) (t2, o2) = do
    let t = fromJust $ infer t1 t2
    r1 <- castOperand (t1, o1) t
    r2 <- castOperand (t2, o2) t
    return (t, (r1, r2))

genTypedBinaryOpM
    :: Expression -> Expression
    -> (Type -> A.Operand -> A.Operand -> Codegen I.Instruction)
    -> Codegen (Maybe (Type, A.Operand))
genTypedBinaryOpM expr1 expr2 f = do
    e1 <- fromJust <$> genExpression expr1
    e2 <- fromJust <$> genExpression expr2
    (t, (o1, o2)) <- inferOperand e1 e2
    i <- f t o1 o2
    r <- addInstr i
    return $ Just (t, r)

genTypedBinaryOp
    :: Expression -> Expression
    -> (Type -> A.Operand -> A.Operand -> I.Instruction)
    -> Codegen (Maybe (Type, A.Operand))
genTypedBinaryOp expr1 expr2 f = genTypedBinaryOpM expr1 expr2 $ \t o1 o2 -> return $ f t o1 o2

genBinaryOp
    :: Expression -> Expression
    -> (A.Operand -> A.Operand -> I.Instruction)
    -> Codegen (Maybe (Type, A.Operand))
genBinaryOp expr1 expr2 f = genTypedBinaryOp expr1 expr2 $ const f

genCmpOp
    :: Expression -> Expression
    -> IP.IntegerPredicate -> FPP.FloatingPointPredicate
    -> Codegen (Maybe (Type, A.Operand))
genCmpOp e1 e2 ip fpp = genTypedBinaryOpM e1 e2 f
    where
        f (PrimaryType TFloat) o1 o2 = return $ I.FCmp fpp o1 o2 []
        f (PrimaryType TDouble) o1 o2 = return $ I.FCmp fpp o1 o2 []
        f (PrimaryType _) o1 o2 = return $ I.ICmp ip o1 o2 []
        f _ o1 o2 = do
            o1i <- addInstr $ I.PtrToInt o1 (T.IntegerType structPtrSize) []
            o2i <- addInstr $ I.PtrToInt o2 (T.IntegerType structPtrSize) []
            return $ I.ICmp ip o1i o2i []

genArithmOp
    :: Expression -> Expression
    -> (A.Operand -> A.Operand -> I.InstructionMetadata -> I.Instruction)
    -> (A.Operand -> A.Operand -> I.InstructionMetadata -> I.Instruction)
    -> Codegen (Maybe (Type, A.Operand))
genArithmOp expr1 expr2 fi ff = genTypedBinaryOp expr1 expr2 f
    where
        f (PrimaryType TFloat) o1 o2 = ff o1 o2 []
        f (PrimaryType TDouble) o1 o2 = ff o1 o2 []
        f (PrimaryType TBoolean) _ _ = error "genExpression"
        f (PrimaryType _) o1 o2 = fi o1 o2 []
        f _ _ _ = error "genExpression"

oneOp :: Type -> A.Operand
oneOp (PrimaryType t) = f t
    where
        f TInt = literalToOp $ LInt 1
        f TLong = literalToOp $ LLong 1
        f _ = error "typed one"
oneOp _ = error "typed one"

load :: A.Operand -> Codegen A.Operand
load ptr = addInstr $ I.Load False ptr Nothing alignment []

store :: A.Operand -> A.Operand -> Codegen ()
store ptr val = addVoidInstr $ I.Store False ptr val Nothing alignment []

genPrePostOp
    :: QualifiedName
    -> (Bool -> Bool -> A.Operand -> A.Operand -> I.InstructionMetadata -> I.Instruction)
    -> Codegen (Type, A.Operand, A.Operand)
genPrePostOp qn f = do
    (t, qPtr) <- fromJust <$> genQualifiedName qn
    qVal <- load qPtr
    newVal <- addInstr $ f False False qVal (oneOp t) []
    store qPtr newVal
    return (t, qVal, newVal)

genPreOp
    :: QualifiedName
    -> (Bool -> Bool -> A.Operand -> A.Operand -> I.InstructionMetadata -> I.Instruction)
    -> Codegen (Maybe (Type, A.Operand))
genPreOp qn f = do
    (t, _, val) <- genPrePostOp qn f
    return $ Just (t, val)

genPostOp
    :: QualifiedName
    -> (Bool -> Bool -> A.Operand -> A.Operand -> I.InstructionMetadata -> I.Instruction)
    -> Codegen (Maybe (Type, A.Operand))
genPostOp qn f = do
    (t, val, _) <- genPrePostOp qn f
    return $ Just (t, val)

genExpression :: Expression -> Codegen (Maybe (Type, A.Operand))
genExpression (Assign qn expr) = do
    (qType, qPtr) <- fromJust <$> genQualifiedName qn
    eRes <- fromJust <$> genExpression expr
    val <- castOperand eRes qType
    store qPtr val
    return $ Just (qType, val)
genExpression (Or e1 e2) = genBinaryOp e1 e2 $ \o1 o2 -> I.Or o1 o2 []
genExpression (And e1 e2) = genBinaryOp e1 e2 $ \o1 o2 -> I.And o1 o2 []
genExpression (Equal e1 e2) = genCmpOp e1 e2 IP.EQ FPP.OEQ
genExpression (Ne e1 e2) = genCmpOp e1 e2 IP.NE FPP.ONE
genExpression (Lt e1 e2) = genCmpOp e1 e2 IP.SLT FPP.OLT
genExpression (Gt e1 e2) = genCmpOp e1 e2 IP.SGT FPP.OGT
genExpression (Le e1 e2) = genCmpOp e1 e2 IP.SLE FPP.OLE
genExpression (Ge e1 e2) = genCmpOp e1 e2 IP.SGE FPP.OGE
genExpression (Plus e1 e2) = genArithmOp e1 e2 (I.Add False False) I.FAdd
genExpression (Minus e1 e2) = genArithmOp e1 e2 (I.Sub False False) I.FSub
genExpression (Mul e1 e2) = genArithmOp e1 e2 (I.Mul False False) I.FMul
genExpression (Div e1 e2) = genArithmOp e1 e2 (I.SDiv False) I.FDiv
genExpression (Mod e1 e2) = genArithmOp e1 e2 I.SRem I.FRem
genExpression (Pos e1) = genExpression e1
genExpression (Neg e1) = genExpression (Minus (Literal $ LInt 0) e1)
genExpression (Not expr) = do
    (t, eRes) <- fromJust <$> genExpression expr
    notE <- addInstr $ I.Sub False False (literalToOp $ LBoolean True) eRes []
    return $ Just (t, notE)
genExpression (PreInc qn) = genPreOp qn I.Add
genExpression (PreDec qn) = genPreOp qn I.Sub
genExpression (PostInc qn) = genPostOp qn I.Add
genExpression (PostDec qn) = genPostOp qn I.Sub
genExpression (QN qn) = do
    qRes <- genQualifiedName qn
    case qRes of
        Nothing -> return Nothing
        Just (qType, qPtr) -> do
            qVal <- load qPtr
            return $ Just (qType, qVal)
genExpression (Literal l) = return $ Just (PrimaryType $ literalType l, literalToOp l)
genExpression Null = return $ Just (NullType, A.ConstantOperand $ C.Null T.VoidType)

call :: String -> [A.Operand] -> I.Instruction
call name params = I.Call False CC.C [] (Right . A.ConstantOperand . C.GlobalReference . A.Name $ name) (map (\s -> (s, [])) params) [] []

genMethodName :: ObjectType -> Method -> String
genMethodName obj mth = List.intercalate "$" $ obj : (show . methodType $ mth) : methodName mth : map (show . paramType) (methodParams mth)

callMethod :: ObjectType -> Method -> A.Operand -> [(Type, A.Operand)] -> Codegen I.Instruction
callMethod obj mth this params = do
    paramsOp <- castList params (map paramType $ methodParams mth)
    return $ call (genMethodName obj mth) $ this : paramsOp

sizeof :: ObjectType -> Codegen A.Operand
sizeof n = gets $ fromJust . Map.lookup n . structSizes

new :: ObjectType -> Codegen A.Operand
new obj = do
    size <- sizeof obj
    ptr <- addInstr $ call "malloc" [size]
    addInstr $ I.BitCast ptr (A.NamedTypeReference . A.Name $ obj) []

genQualifiedName :: QualifiedName -> Codegen (Maybe (Type, A.Operand))
genQualifiedName (FieldAccess qn field) = do
    (t', op) <- fromJust <$> genQualifiedName qn
    let t = objType t'
    (ft, i) <- fromRight <$> findField t field
    retOp <- addInstr $ I.GetElementPtr False op [structFieldAddr i] []
    return $ Just (ft, retOp) 
genQualifiedName (MethodCall qn mthName params) = do
    (t', op) <- fromJust <$> genQualifiedName qn
    let t = objType t'
    paramsOp <- fmap (map fromJust) . mapM genExpression $ params
    mth <- fromRight <$> findMethod t mthName (fst $ unzip paramsOp) (\m -> methodType m /= Constructor)
    instr <- callMethod t mth op paramsOp
    case methodType mth of
        Void -> do
            addVoidInstr instr
            return Nothing
        Constructor -> error "genQualifiedName"
        ReturnType rt -> do
            retOp <- addInstr instr
            return $ Just (rt, retOp)
genQualifiedName (Var var) = do
    mv <- lookupLocalVar var
    case mv of
        Just v -> return $ Just v
        Nothing -> genQualifiedName $ FieldAccess This var
genQualifiedName (New ot params) = do
    paramsOp <- fmap (map fromJust) . mapM genExpression $ params
    mth <- fromRight <$> findMethod ot ot (fst $ unzip paramsOp) (\m -> methodType m == Constructor)
    ptr <- new ot
    instr <- callMethod ot mth ptr paramsOp
    retOp <- addInstr instr
    return $ Just (ObjectType ot, retOp)
genQualifiedName This = do
    cls <- getClassM
    let op = A.LocalReference $ A.Name "this"
    return $ Just (ObjectType . className $ cls, op)

nextLabel :: String -> Codegen A.Name
nextLabel l = do
    i' <- gets lastLabel
    let i = i' + 1
    modify $ \s -> s { lastLabel = i }
    return $ A.Name $ l ++ "." ++ show i

popInstructions :: Codegen [A.Named I.Instruction]
popInstructions = do
    i <- gets instructions
    modify $ \s -> s { instructions = [] }
    return i

getBBName :: A.BasicBlock -> A.Name
getBBName (A.BasicBlock name _ _) = name

genStuct :: Class -> A.Definition
genStuct cls = A.TypeDefinition (A.Name $ className cls) $ Just $ T.StructureType False $ map (mapType . varType) $ classFields cls

goToBlock :: A.Name -> Codegen A.BasicBlock
goToBlock name = do
    exitLabel <- nextLabel "GoToBlock"
    return $ A.BasicBlock exitLabel [] $ I.Do $ I.Br name []

type EStatement = Either (A.BasicBlock -> [A.BasicBlock]) [A.BasicBlock]

genIfConsAltOk
    :: A.Name -> A.Operand -> [A.Named I.Instruction]
    -> [A.BasicBlock]
    -> [A.BasicBlock]
    -> [A.BasicBlock]
genIfConsAltOk ifLabel flag calcFlag consBlocks altBlocks = A.BasicBlock ifLabel calcFlag (I.Do $ I.CondBr flag consBlockName altBlockName []) : (consBlocks ++ altBlocks)
    where
        consBlockName = getBBName . head $ consBlocks
        altBlockName = getBBName . head $ altBlocks

genIfCons
    :: A.Name -> A.Operand -> [A.Named I.Instruction]
    -> (A.BasicBlock -> [A.BasicBlock]) -> A.BasicBlock -> [A.BasicBlock]
genIfCons ifLabel flag calcFlag getConsBlocks finBlock = A.BasicBlock ifLabel calcFlag (I.Do $ I.CondBr flag consBlockName finBlockName []) : consBlocks
    where
        finBlockName = getBBName finBlock
        consBlocks = getConsBlocks finBlock
        consBlockName = getBBName . head $ consBlocks

genIfConsAlt
    :: A.Name -> A.Operand -> [A.Named I.Instruction]
    -> (A.BasicBlock -> [A.BasicBlock])
    -> (A.BasicBlock -> [A.BasicBlock])
    -> A.BasicBlock -> [A.BasicBlock]
genIfConsAlt ifLabel flag calcFlag getConsBlocks getAltBlocks finBlock = genIfConsAltOk ifLabel flag calcFlag consBlocks altBlocks
    where
        consBlocks = getConsBlocks finBlock
        altBlocks = getAltBlocks finBlock

getExpressionRes :: Expression -> Codegen (A.Operand, [A.Named I.Instruction])
getExpressionRes e = do
    op <- (snd . fromJust) <$> genExpression e
    calc <- popInstructions
    return (op, calc)

genIf :: If -> Codegen EStatement
genIf (If cond cons alt) = do
    ifLabel <- nextLabel "If"
    (flag, calcFlag) <- getExpressionRes cond
    consBlocks' <- genStatement cons
    let genIfCons' = genIfCons ifLabel flag calcFlag
        genIfConsAlt' = genIfConsAlt ifLabel flag calcFlag
    case (consBlocks', alt) of
        (Left getCons, Nothing) -> return . Left $ genIfCons' getCons
        (Left getCons, Just alt') -> do    
            altBlocks' <- genStatement alt'
            case altBlocks' of
                Left getAlt -> return . Left $ genIfConsAlt' getCons getAlt
                Right altBlocks -> return . Left $ genIfConsAlt' getCons (const altBlocks)
        (Right consBlocks, Nothing) -> return . Left $ genIfCons' $ const consBlocks
        (Right consBlocks, Just alt') -> do
            altBlocks' <- genStatement alt'
            case altBlocks' of
                Left getAlt -> return . Left $ genIfConsAlt' (const consBlocks) getAlt
                Right altBlocks -> return . Right $ genIfConsAltOk ifLabel flag calcFlag consBlocks altBlocks

genWhile :: While -> Codegen (A.BasicBlock -> [A.BasicBlock])
genWhile (While cond st) = do
    whileLabel <- nextLabel "While"
    lastBlock <- goToBlock whileLabel
    (flag, calcFlag) <- getExpressionRes cond
    stBlocks' <- genStatement st
    let stBlocks = case stBlocks' of
            Left f -> f lastBlock
            Right b -> b
        stBlockName = getBBName . head $ stBlocks
    return $ \finBlock ->
        let finBlockName = getBBName finBlock in
        [A.BasicBlock whileLabel calcFlag (I.Do $ I.CondBr flag stBlockName finBlockName [])] ++ stBlocks ++ [lastBlock]

genExpressionList :: [Expression] -> Codegen ()
genExpressionList = mapM_ genExpression

genForInit :: ForInit -> Codegen ()
genForInit (ForInitEL l) = genExpressionList l
genForInit _ = undefined

brBlock :: A.Name -> A.Name -> [A.Named I.Instruction] -> A.BasicBlock
brBlock name next instr = A.BasicBlock name instr $ I.Do $ I.Br next []

genFor :: For -> Codegen (A.BasicBlock -> [A.BasicBlock])
genFor (For fInit cond inc st) = do
    addScope
    initLabel <- nextLabel "ForInit"
    genForInit fInit
    calcInit <- popInstructions
    forLabel <- nextLabel "For"
    (flag, calcFlag) <- getExpressionRes cond
    stBlocks' <- genStatement st
    incLabel <- nextLabel "ForInc"
    genExpressionList inc
    calcInc <- popInstructions
    let initBlock = brBlock initLabel forLabel calcInit
        incBlock = brBlock incLabel forLabel calcInc
        stBlocks = case stBlocks' of
            Left f -> f incBlock
            Right b -> b
        stBlockName = getBBName . head $ stBlocks
    removeScope
    return $ \finBlock ->
        let finBlockName = getBBName finBlock in
        [initBlock] ++ [A.BasicBlock forLabel calcFlag (I.Do $ I.CondBr flag stBlockName finBlockName [])] ++ stBlocks ++ [incBlock]

genStatement :: Statement -> Codegen EStatement
genStatement _ = undefined

genBlockStatement :: BlockStatement -> Codegen [A.BasicBlock]
genBlockStatement _ = undefined

genMethod :: Method -> Codegen A.Definition
genMethod _ = undefined

genPreInit :: Class -> Codegen [A.Named I.Instruction]
genPreInit _ = undefined

genClass :: Class -> Codegen [A.Definition]
genClass cls = do
    modify $ \s -> s { csClass = Just cls }
    modify $ \s -> s { csClass = Nothing }
    return []

genProgram :: Program -> Codegen A.Module
genProgram p = do
    modify $ \s -> s { csProgram = Just p }
    defs <- concat <$> forM p genClass
    modify $ \s -> s { csProgram = Nothing }
    return A.defaultModule { A.moduleDefinitions = defs }
    
-- import LLVM.General.AST
-- import LLVM.General.AST.Global as G
-- import qualified LLVM.General.AST.Type as T
-- 
-- import Control.Monad.Identity
-- import Control.Monad.State
-- import Data.Maybe
-- import qualified Data.Map as Map
-- 
-- import AST
-- 
-- type SymbolTable = Map.Map String Operand
-- 
-- type SemanticError = String
-- 
-- data CodegenState
--     = CodegenState
--     { program :: Program
--     , symbolTable :: SymbolTable
--     , errors :: [SemanticError]
--     } deriving Show
-- 
-- type CodegenT m a = StateT CodegenState m a
-- 
-- type Codegen a = CodegenT Identity a
-- 
-- classStruct :: Class -> Codegen T.Type
-- classStruct cls = do
--     fieldTypes <- fmap catMaybes $ mapM fieldStruct $ classFields cls
--     return $ StructureType False fieldTypes
-- 
-- fieldStruct :: Variable -> Codegen (Maybe T.Type)
-- fieldStruct = mapType . varType
-- 
-- mapMethodType :: AST.Class -> AST.MethodType -> Codegen (Maybe T.Type)
-- mapMethodType _ (ReturnType t) = mapType t
-- mapMethodType _ Void = return $ Just T.VoidType
-- mapMethodType cls Constructor = return . Just . NamedTypeReference . Name . className $ cls
-- 
-- mapType :: AST.Type -> Codegen (Maybe T.Type)
-- mapType (PrimaryType t) = return . Just . mapPrimaryType $ t
-- mapType (ObjectType t) = do
--     defined <- checkTypeDefined t
--     return $ if defined then Just $ NamedTypeReference (Name t) else Nothing
-- 
-- mapPrimaryType :: PrimaryType -> T.Type
-- mapPrimaryType TBoolean = IntegerType 1
-- mapPrimaryType TByte = IntegerType 8
-- mapPrimaryType TShort = IntegerType 16
-- mapPrimaryType TInt = IntegerType 32
-- mapPrimaryType TLong = IntegerType 64
-- mapPrimaryType TFloat = FloatingPointType 32 IEEE
-- mapPrimaryType TDouble = FloatingPointType 64 IEEE
-- 
-- mapParameter :: AST.Parameter -> Codegen (Maybe G.Parameter)
-- mapParameter param = do
--     t <- mapType . paramType $ param
--     return $ if isNothing t then Nothing else Just $ G.Parameter (fromJust t) (Name . paramName $ param) []
--     
-- 
-- genMethod :: Class -> Method -> Codegen (Maybe Global)
-- genMethod cls mth = do
--     maybeFuncType <- mapMethodType cls . methodType $ mth
--     if isNothing maybeFuncType then return Nothing else do
--         let funcType = fromJust maybeFuncType
--         return . Just $ functionDefaults
--             { returnType = funcType
--             }
