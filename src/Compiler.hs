module Main where

import qualified Lexer as L

import Checker
import Parser
import Control.Monad.Except

main :: IO ()
main = do
    s <- getLine
    print $ case L.runAlex s parse of
        Right p -> show . runExcept . checkProgram $ p
        Left err -> err
