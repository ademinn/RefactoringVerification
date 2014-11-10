module Main where

import qualified Lexer as L
import Parser
import Semantic
import Reduction

import Control.Monad.Except
import Control.Applicative
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let originalFile = args !! 0
        modifiedFile = args !! 1
        fromClass = args !! 2
        fromMethod = args !! 3
        toClass = args !! 4
        toMethod = args !! 5
    original <- readFile originalFile
    modified <- readFile modifiedFile
    let originalRes = L.runAlex original parse
        modifiedRes = L.runAlex modified parse
    case (,) <$> originalRes <*> modifiedRes of
        Right (originalPT, modifiedPT) -> print "ok"
        Left err -> print err
