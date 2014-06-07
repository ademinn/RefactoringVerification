module Main where

import Parser
import Lexer
import Checker
import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity

main :: IO ()
main = do
    p <- (parse . alexScanTokens) <$> getLine
    putStrLn . show . runIdentity . runWriterT . runStateT (check p) $ defaultAnalyzerState
