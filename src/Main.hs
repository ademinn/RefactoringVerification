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
        Right (originalPT, modifiedPT) -> do
            let originalProgramRes = runExcept $ checkProgram originalPT
                modifiedProgramRes = runExcept $ checkProgram modifiedPT
            case (,) <$> originalProgramRes <*> modifiedProgramRes of
                Right (originalProgram, modifiedProgram) ->
                    case checkMethodExtraction originalProgram modifiedProgram fromClass fromMethod toClass toMethod of
                        Nothing -> putStrLn "refactoring verified successfully"
                        Just (actual, expected) -> do
                            putStrLn $ "actual: " ++ show actual
                            putStrLn $ "expected: " ++ show expected
                Left err -> print err
        Left err -> print err
