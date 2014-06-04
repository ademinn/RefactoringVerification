module Codegen where

import LLVM.General.AST
import LLVM.General.AST.Global as G
import qualified LLVM.General.AST.Type as T

import Control.Monad.Identity
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map

import AST

type SymbolTable = Map.Map String Operand

type SemanticError = String

data CodegenState
    = CodegenState
    { program :: Program
    , symbolTable :: SymbolTable
    , errors :: [SemanticError]
    } deriving Show

type CodegenT m a = StateT CodegenState m a

type Codegen a = CodegenT Identity a

newError :: SemanticError -> Codegen ()
newError err = do
    errs <- gets errors
    modify $ \s -> s { errors = errs ++ [err] } 

checkTypeDefined :: ObjectType -> Codegen Bool
checkTypeDefined typeName = do
    classNames <- gets $ \s -> map className $ program s
    let defined = typeName `elem` classNames
    if defined then return True else do
        newError $ "Type " ++ typeName ++ " undefined"
        return False

generate :: Program -> Module
generate _ = undefined

classStruct :: Class -> Codegen T.Type
classStruct cls = do
    fieldTypes <- fmap catMaybes $ mapM fieldStruct $ classFields cls
    return $ StructureType False fieldTypes

fieldStruct :: Variable -> Codegen (Maybe T.Type)
fieldStruct = mapType . varType

mapMethodType :: AST.Class -> AST.MethodType -> Codegen (Maybe T.Type)
mapMethodType _ (ReturnType t) = mapType t
mapMethodType _ Void = return $ Just T.VoidType
mapMethodType cls Constructor = return . Just . NamedTypeReference . Name . className $ cls

mapType :: AST.Type -> Codegen (Maybe T.Type)
mapType (PrimaryType t) = return . Just . mapPrimaryType $ t
mapType (ObjectType t) = do
    defined <- checkTypeDefined t
    return $ if defined then Just $ NamedTypeReference (Name t) else Nothing

mapPrimaryType :: PrimaryType -> T.Type
mapPrimaryType TBoolean = IntegerType 1
mapPrimaryType TByte = IntegerType 8
mapPrimaryType TShort = IntegerType 16
mapPrimaryType TInt = IntegerType 32
mapPrimaryType TLong = IntegerType 64
mapPrimaryType TFloat = FloatingPointType 32 IEEE
mapPrimaryType TDouble = FloatingPointType 64 IEEE

mapParameter :: AST.Parameter -> Codegen (Maybe G.Parameter)
mapParameter param = do
    t <- mapType . paramType $ param
    return $ if isNothing t then Nothing else Just $ G.Parameter (fromJust t) (Name . paramName $ param) []
    

genMethod :: Class -> Method -> Codegen (Maybe Global)
genMethod cls mth = do
    maybeFuncType <- mapMethodType cls . methodType $ mth
    if isNothing maybeFuncType then return Nothing else do
        let funcType = fromJust maybeFuncType
        return . Just $ functionDefaults
            { returnType = funcType
            }
