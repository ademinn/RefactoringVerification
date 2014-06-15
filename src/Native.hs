module Native where

import qualified LLVM.General.AST as A
import qualified LLVM.General.AST.Type as T
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Instruction as I
import qualified LLVM.General.AST.Global as G

import Data.Char

import Type
import AST
import Codegen

consoleWriter :: Class
consoleWriter = Class
    { className = "ConsoleWriter"
    , classFields = []
    , classMethods = [consoleWriterConstructor, writeInt]
    }

consoleWriterConstructor :: Method
consoleWriterConstructor = Method
    { methodType = Constructor
    , methodName = "ConsoleWriter"
    , methodParams = []
    , methodBlock = []
    }

writeInt :: Method
writeInt = Method
    { methodType = Void
    , methodName = "writeInt"
    , methodParams = [Parameter (PrimaryType TInt) "value"]
    , methodBlock = []
    }

stringConstant :: String -> C.Constant
stringConstant s = C.Array (T.IntegerType 8) $ map (C.Int 8 . toInteger . ord) s

constants :: [(String, T.Type, C.Constant)]
constants = [("%d", T.ArrayType 2 (T.IntegerType 8), stringConstant "%d")]

genWriteInt :: Codegen [A.BasicBlock]
genWriteInt = do
    label <- nextLabel "WriteInt"
    strPtr <- addInstr $ I.GetElementPtr True (A.ConstantOperand $ C.GlobalReference $ A.Name "%d") [A.ConstantOperand $ C.Int 32 0, A.ConstantOperand $ C.Int 32 0] []
    let i = call "printf" [strPtr, A.LocalReference $ A.Name "value"]
    addVoidInstr i
    instr <- popInstructions
    return [A.BasicBlock label instr $ I.Do $ I.Ret Nothing []]

genConsoleWriterConstructor :: Codegen [A.BasicBlock]
genConsoleWriterConstructor = do
    label <- nextLabel "ConsoleWriter"
    return [emptyBlock label $ I.Ret (Just $ A.LocalReference $ A.Name "this") []]

genConsoleWriter :: Codegen [A.Definition]
genConsoleWriter = do
    let cls = consoleWriter
        struct = genStruct cls
        globalVars = map (\(name, t, c) -> A.GlobalDefinition $ A.globalVariableDefaults { G.name = A.Name name, G.type' = t, G.initializer = Just c}) constants
    wiBlocks <- genWriteInt
    let wi = genMethodDefinition cls writeInt wiBlocks
    cwcBlocks <- genConsoleWriterConstructor
    let cwc = genMethodDefinition cls consoleWriterConstructor cwcBlocks
    return $ globalVars ++ [struct, cwc, wi]
