module Codegen where

import qualified LLVM.General.AST as A
import qualified LLVM.General.AST.Type as T
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Instruction as I
import qualified LLVM.General.AST.CallingConvention as CC

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

mapType :: Type -> T.Type
mapType (PrimaryType t) = mapPrimaryType t
mapType (ObjectType t) = T.NamedTypeReference (A.Name t) 
mapType NullType = error "mapType" -- FIXME

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

structFieldAddr :: Int -> A.Operand
structFieldAddr i = A.ConstantOperand . C.Int structPtrSize $ toInteger i

genExpression :: Expression -> Codegen (Maybe (Type, A.Operand))
genExpression _ = undefined

castList :: [(Type, A.Operand)] -> [Type] -> Codegen [A.Operand]
castList _ _ = undefined

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
