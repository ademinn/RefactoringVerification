module Main where

import qualified System.Environment as Env

import qualified Lexer as L

import Parser
import Checker
import Generator
import Codegen
import Analyzer
import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Monad.Error

import qualified LLVM.General.Module as M
import qualified LLVM.General.Context as C

codeExtension :: String
codeExtension = ".java"

assemblyExtension :: String
assemblyExtension = ".ll"

liftError :: ErrorT String IO a -> IO a
liftError = runErrorT >=> either fail return

splitLast :: Int -> String -> (String, String)
splitLast i s = splitAt ((length s) - i) s

withArgs :: [String] -> (String -> IO (Maybe String)) -> IO ()
withArgs [] _ = putStrLn "no file specified"
withArgs (n:[]) f = do
    let (fn, ext) = splitLast (length codeExtension) n
    if ext /= codeExtension then putStrLn $ "file must have \"" ++ codeExtension ++ "\" extension" else do
        code <- readFile n
        res <- f code
        case res of
            Nothing -> return ()
            Just s -> writeFile (fn ++ assemblyExtension) s
withArgs _ _ = putStrLn "more than one file specified"

main :: IO ()
main = do
    args <- Env.getArgs
    withArgs args $ \s -> do
        let parseRes = L.runAlex s parse
        case parseRes of
            Left err -> do
                putStrLn err
                return Nothing
            Right p' -> do
                let p = map addEmptyConstructor p'
                case runIdentity . execWriterT . runStateT (check p) $ defaultAnalyzerState of
                    [] -> do
                        let astModule = evalState (genProgram p) defaultCodegenState
                        C.withContext $ \context ->
                            liftError $ M.withModuleFromAST context astModule $ \m ->
                                Just <$> M.moduleLLVMAssembly m
                    l -> do
                        putStrLn $ unlines l
                        return Nothing
