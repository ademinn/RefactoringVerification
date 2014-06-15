module Main where

import qualified System.Environment as Env

import Foreign.Ptr

import LLVM.General.AST
import LLVM.General.Module
import LLVM.General.Context
import LLVM.General.ExecutionEngine

import Control.Monad.Error

foreign import ccall "dynamic" haskFun :: FunPtr (IO ()) -> (IO ())

run :: FunPtr a -> IO ()
run f = haskFun (castFunPtr f :: FunPtr (IO ()))

liftError :: ErrorT b IO a -> IO a
liftError err = do
    res <- runErrorT err
    case res of
        Left _ -> error "err"
        Right b -> return b

assemblyExtension :: String
assemblyExtension = ".ll"

splitLast :: Int -> String -> (String, String)
splitLast i s = splitAt ((length s) - i) s

withArgs :: [String] -> (String -> IO ()) -> IO ()
withArgs [] _ = putStrLn "no file specified"
withArgs (n:[]) f = do
    let (fn, ext) = splitLast (length assemblyExtension) n
    if ext /= assemblyExtension then putStrLn $ "file must have \"" ++ assemblyExtension ++ "\" extension" else do
        assembly <- readFile n
        f assembly
withArgs _ _ = putStrLn "more than one file specified"

main :: IO ()
main = do
    args <- Env.getArgs
    withArgs args $ \s -> withContext $ \context -> liftError $ withModuleFromLLVMAssembly context s $ \m ->
        withJIT context 0 $ \engine ->
            withModuleInEngine engine m $ \ee -> do
                mainF <- getFunction ee (Name "main")
                case mainF of
                    Just f -> do
                        run f
                        return ()
                    Nothing -> putStrLn "main function not found"
