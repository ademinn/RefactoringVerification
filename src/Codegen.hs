module Codegen where

import qualified LLVM.General.AST as A
import qualified LLVM.General.AST.Type as T
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Instruction as I
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.AddrSpace as AddrSpace
import qualified LLVM.General.AST.Global as G

import Control.Monad.State

import Data.Maybe
import Data.Word
import qualified Data.List as List
import qualified Data.Map as Map

import Debug.Trace

import Scope
import AST
import Analyzer
import Type

structPtrSize :: Word32
structPtrSize = 32

alignment :: Word32
alignment = 0

addrSpace :: AddrSpace.AddrSpace
addrSpace = AddrSpace.AddrSpace 0

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
    , loops :: [(A.Name, A.Name)]
    } deriving Show

defaultCodegenState :: CodegenState
defaultCodegenState = CodegenState
    { csProgram = Nothing
    , csScope = []
    , csClass = Nothing
    , csMethod = Nothing
    , lastInd = -1
    , instructions = []
    , structSizes = Map.empty
    , lastLabel = 0
    , loops = []
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

addLoop :: (A.Name, A.Name) -> Codegen ()
addLoop n = modify $ \s -> s { loops = n : loops s }

removeLoop :: Codegen ()
removeLoop = modify $ \s -> s { loops = tail . loops $ s }

lastLoopEnd :: Codegen A.Name
lastLoopEnd = gets $ snd . head . loops

lastLoopNext :: Codegen A.Name
lastLoopNext = gets $ fst . head . loops

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

nullPrimaryValue :: PrimaryType -> Literal
nullPrimaryValue TBoolean = LBoolean False 
nullPrimaryValue TInt = LInt 0
nullPrimaryValue TLong = LLong 0
nullPrimaryValue TFloat = LFloat 0.0
nullPrimaryValue TDouble = LDouble 0.0
nullPrimaryValue _ = error "null primary value"

nullValue :: Type -> Expression
nullValue (ObjectType _) = Null
nullValue (PrimaryType pt) = Literal $ nullPrimaryValue pt
nullValue NullType = error "null value"

mapType :: Type -> T.Type
mapType (PrimaryType t) = mapPrimaryType t
mapType (ObjectType t) = getPointerType . T.NamedTypeReference . A.Name $ t
mapType NullType = error "mapType"

mapMethodType :: Class -> MethodType -> T.Type
mapMethodType _ (ReturnType t) = mapType t
mapMethodType _ Void = T.VoidType
mapMethodType cls Constructor = getPointerType . A.NamedTypeReference . A.Name . className $ cls

nextName :: Codegen A.Name
nextName = do
    i <- gets lastInd
    let i' = i + 1
    modify $ \s -> s { lastInd = i' }
    return $ A.UnName i'

addNamedInstr :: A.Name -> I.Instruction -> Codegen A.Operand
addNamedInstr n instr = do
    modify $ \s -> s { instructions = instructions s ++ [n A.:= instr] }
    return $ A.LocalReference n

addInstr :: I.Instruction -> Codegen A.Operand
addInstr instr = do
    n <- nextName
    addNamedInstr n instr

addVoidInstr :: I.Instruction -> Codegen ()
addVoidInstr instr = modify $ \s -> s { instructions = instructions s ++ [A.Do instr] }

fromRight :: String -> Either a b -> b
fromRight _ (Right b) = b
fromRight err (Left _) = error $ "fromRight " ++ err

objType :: Type -> ObjectType
objType (ObjectType t) = t
objType _ = error "objType"

getPointerType :: T.Type -> T.Type
getPointerType t = T.PointerType t addrSpace

structFieldAddr :: Int -> A.Operand
structFieldAddr i = A.ConstantOperand . C.Int structPtrSize $ toInteger i

nullConstant :: ObjectType -> A.Operand
nullConstant t = A.ConstantOperand $ C.Null $ mapType $ ObjectType t

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

call :: String -> [A.Operand] -> I.Instruction
call name params = I.Call False CC.C [] (Right . A.ConstantOperand . C.GlobalReference . A.Name $ name) (map (\s -> (s, [])) params) [] []

genMethodName :: ObjectType -> Method -> String
genMethodName obj mth = List.intercalate "$" $ obj : (show . methodType $ mth) : methodName mth : map (show . paramType) (methodParams mth)

sizeof :: ObjectType -> Codegen A.Operand
sizeof n = do
    s' <- addInstr $ I.GetElementPtr False (nullConstant n) [oneOp $ PrimaryType TInt] []
    addInstr $ I.PtrToInt s' (T.IntegerType structPtrSize) []

new :: ObjectType -> Codegen A.Operand
new obj = do
    size <- sizeof obj
    ptr <- addInstr $ call "malloc" [size]
    addInstr $ I.BitCast ptr (getPointerType . A.NamedTypeReference . A.Name $ obj) []

alloca :: T.Type -> I.Instruction
alloca t = I.Alloca t Nothing alignment []

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

emptyBlock :: A.Name -> I.Terminator -> A.BasicBlock
emptyBlock n t = A.BasicBlock n [] $ A.Do t

brBlock :: A.Name -> A.Name -> [A.Named I.Instruction] -> A.BasicBlock
brBlock name next instr = A.BasicBlock name instr $ I.Do $ I.Br next []

toList :: a -> [a]
toList a = [a]

genBrBlock :: A.Name -> [A.Named I.Instruction] -> A.Name -> [A.BasicBlock]
genBrBlock n instr finBlockName = [brBlock n finBlockName instr]

genParam :: Parameter -> Codegen ()
genParam (Parameter t n) = do
    ptr <- addInstr $ alloca . mapType $ t
    store ptr $ A.LocalReference $ A.Name n
    void $ newLocalVar n (t, ptr)

genParams :: [Parameter] -> Codegen ()
genParams params = forM_ params genParam

mapParam :: Parameter -> G.Parameter
mapParam (Parameter t n) = G.Parameter (mapType t) (A.Name n) []

genStruct :: Class -> A.Definition
genStruct (Class name fields _) = A.TypeDefinition (A.Name name) $
    Just $ T.StructureType False $ map (mapType . varType) fields

mallocDecl :: A.Definition
mallocDecl = A.GlobalDefinition G.functionDefaults
    { G.returnType = T.PointerType (T.IntegerType 8) addrSpace
    , G.name = A.Name "malloc"
    , G.parameters = ([G.Parameter (T.IntegerType 32) (A.Name "size") []], False)
    }

printfDecl :: A.Definition
printfDecl = A.GlobalDefinition G.functionDefaults
    { G.returnType = T.IntegerType 32
    , G.name = A.Name "printf"
    , G.parameters = ([G.Parameter (getPointerType $ T.IntegerType 8) (A.Name "format") []], True)
    }

genMethodDefinition :: Class -> Method -> [A.BasicBlock] -> A.Definition
genMethodDefinition cls mth@(Method mt _ mp _) mthBlocks = trace (unlines $ map (\b -> "-> " ++ show b) mthBlocks) $ A.GlobalDefinition G.functionDefaults
        { G.returnType = mapMethodType cls mt
        , G.name = A.Name $ genMethodName (className cls) mth
        , G.parameters = (map mapParam params, False)
        , G.basicBlocks = mthBlocks
        }
    where
        params = Parameter (ObjectType . className $ cls) "this" : mp
