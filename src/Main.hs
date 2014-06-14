module Main where

import Parser
import Lexer
import Checker
import Codegen
import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Monad.Error

import LLVM.General.Module
import LLVM.General.Context

liftError :: ErrorT String IO a -> IO a
liftError = runErrorT >=> either fail return

main :: IO ()
main = do
    p <- (parse . alexScanTokens) <$> getLine
    let m = evalState (genProgram p) defaultCodegenState
    -- . runIdentity . runWriterT . runStateT (check p) $ defaultAnalyzerState
    withContext $ \context -> liftError $ withModuleFromAST context m $ putStrLn <=< moduleLLVMAssembly
