module Native where

import qualified LLVM.General.AST as A
import qualified LLVM.General.AST.Type as T
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Instruction as I
import qualified LLVM.General.AST.Global as G

import Control.Applicative
import Control.Monad

import Data.Char

import Type
import AST
import Codegen

intWF :: String
intWF = "intWriteFormat"

longWF :: String
longWF = "longWriteFormat"

doubleWF :: String
doubleWF = "doubleWriteFormat"

byteRF :: String
byteRF = "byteReadFormat"

shortRF :: String
shortRF = "shortReadFormat"

intRF :: String
intRF = "intReadFormat"

longRF :: String
longRF = "longReadFormat"

floatRF :: String
floatRF = "floatReadFormat"

doubleRF :: String
doubleRF = "doubleReadFormat"

stringConstant :: String -> C.Constant
stringConstant s = C.Array (T.IntegerType 8) $ map (C.Int 8 . toInteger . ord) s

genStringConstant :: String -> (T.Type, C.Constant)
genStringConstant s = (T.ArrayType (fromInteger . toInteger . length $ s) (T.IntegerType 8), stringConstant s)

ptrToString :: String -> I.Instruction
ptrToString name = I.GetElementPtr False (A.ConstantOperand $ C.GlobalReference $ A.Name name) [zero, zero] []
    where zero = A.ConstantOperand $ C.Int 32 0

constants :: [(String, T.Type, C.Constant)]
constants = map f
        [ (intWF, "%d\n")
        , (longWF, "%ld\n")
        , (doubleWF, "%f\n")
        , (byteRF, "%hhd")
        , (shortRF, "%hd")
        , (intRF, "%d")
        , (longRF, "%lld")
        , (floatRF, "%f")
        , (doubleRF, "%lf")
        ]
    where
        f (name, str) =
            let (t, c) = genStringConstant (str ++ "\0") in
            (name, t, c)

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

scanfDecl :: A.Definition
scanfDecl = A.GlobalDefinition G.functionDefaults
    { G.returnType = T.IntegerType 32
    , G.name = A.Name "scanf"
    , G.parameters = ([G.Parameter (getPointerType $ T.IntegerType 8) (A.Name "format") []], True)
    }

writeParam :: String
writeParam = "value"

consoleWriter :: Class
consoleWriter = Class
    { className = "ConsoleWriter"
    , classFields = []
    , classMethods =
        [ consoleWriterConstructor
        , writeInt
        , writeLong
        , writeDouble
        ]
    }

consoleWriterConstructor :: Method
consoleWriterConstructor = Method
    { methodType = Constructor
    , methodName = "ConsoleWriter"
    , methodParams = []
    , methodBlock = []
    }

writeMethod :: String -> PrimaryType -> Method
writeMethod name t = Method
    { methodType = Void
    , methodName = name
    , methodParams = [Parameter (PrimaryType t) writeParam]
    , methodBlock = []
    }

writeInt :: Method
writeInt = writeMethod "writeInt" TInt

writeLong :: Method
writeLong = writeMethod "writeLong" TLong

writeDouble :: Method
writeDouble = writeMethod "writeDouble" TDouble

genWriteMethod :: Method -> String -> Codegen A.Definition
genWriteMethod mth format = do
    label <- nextLabel "Write"
    strPtr <- addInstr $ ptrToString format
    let i = call "printf" [strPtr, A.LocalReference $ A.Name writeParam]
    addVoidInstr i
    instr <- popInstructions
    let blocks = [A.BasicBlock label instr $ I.Do $ I.Ret Nothing []]
    return $ genMethodDefinition consoleWriter mth blocks

genConsoleWriterConstructor :: Codegen [A.BasicBlock]
genConsoleWriterConstructor = do
    label <- nextLabel "ConsoleWriter"
    return [emptyBlock label $ I.Ret (Just $ A.LocalReference $ A.Name "this") []]

genConsoleWriter :: Codegen [A.Definition]
genConsoleWriter = do
    let cls = consoleWriter
        struct = genStruct cls
    mths <- mapM (uncurry genWriteMethod)
        [ (writeInt, intWF)
        , (writeLong, longWF)
        , (writeDouble, doubleWF)
        ]
    cwcBlocks <- genConsoleWriterConstructor
    let cwc = genMethodDefinition cls consoleWriterConstructor cwcBlocks
    return $ [struct, cwc] ++ mths

consoleReader :: Class
consoleReader = Class
    { className = "ConsoleReader"
    , classFields = []
    , classMethods =
        [ consoleReaderConstructor
        , readByte
        , readShort
        , readInt
        , readLong
        , readFloat
        , readDouble
        ]
    }

consoleReaderConstructor :: Method
consoleReaderConstructor = Method
    { methodType = Constructor
    , methodName = "ConsoleReader"
    , methodParams = []
    , methodBlock = []
    }

readMethod :: String -> PrimaryType -> Method
readMethod name t = Method
    { methodType = ReturnType . PrimaryType $ t
    , methodName = name
    , methodParams = []
    , methodBlock = []
    }

readByte :: Method
readByte = readMethod "readByte" TByte

readShort :: Method
readShort = readMethod "readShort" TShort

readInt :: Method
readInt = readMethod "readInt" TInt

readLong :: Method
readLong = readMethod "readLong" TLong

readFloat :: Method
readFloat = readMethod "readFloat" TFloat

readDouble :: Method
readDouble = readMethod "readDouble" TDouble

genReadMethod :: Method -> String -> T.Type -> Codegen A.Definition
genReadMethod mth format t = do
    label <- nextLabel "Read"
    strPtr <- addInstr $ ptrToString format
    resPtr <- addInstr $ alloca t
    let i = call "scanf" [strPtr, resPtr]
    addVoidInstr i
    res <- load resPtr
    instr <- popInstructions
    let blocks = [A.BasicBlock label instr $ I.Do $ I.Ret (Just res) []]
    return $ genMethodDefinition consoleReader mth blocks

genConsoleReaderConstructor :: Codegen [A.BasicBlock]
genConsoleReaderConstructor = do
    label <- nextLabel "ConsoleReader"
    return [emptyBlock label $ I.Ret (Just $ A.LocalReference $ A.Name "this") []]

genConsoleReader :: Codegen [A.Definition]
genConsoleReader = do
    let cls = consoleReader
        struct = genStruct cls
    mths <- mapM (\(mth, format, t) -> genReadMethod mth format t)
        [ (readByte, byteRF, T.IntegerType 8)
        , (readShort, shortRF, T.IntegerType 16)
        , (readInt, intRF, T.IntegerType 32)
        , (readLong, longRF, T.IntegerType 64)
        , (readFloat, floatRF, T.FloatingPointType 32 T.IEEE)
        , (readDouble, doubleRF, T.FloatingPointType 64 T.IEEE)
        ]
    crcBlocks <- genConsoleReaderConstructor
    let crc = genMethodDefinition cls consoleReaderConstructor crcBlocks
    return $ [struct, crc] ++ mths

nativeClasses :: [Class]
nativeClasses = [consoleWriter, consoleReader]

nativeDefinitions :: Codegen [A.Definition]
nativeDefinitions = do
    let globalVars = map
            (\(name, t, c) -> A.GlobalDefinition $ A.globalVariableDefaults
                { G.name = A.Name name
                , G.type' = t
                , G.initializer = Just c
                })
            constants
    (\defs -> mallocDecl : printfDecl : scanfDecl : globalVars ++ concat defs) <$> sequence
        [ genConsoleWriter
        , genConsoleReader
        ]
