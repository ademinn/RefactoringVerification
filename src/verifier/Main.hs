module Main where

import FJ
import Reduction

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
    original <- parse <$> readFile originalFile
    modified <- parse <$> readFile modifiedFile
    case (,) <$> original <*> modified of
        Right (originalProgram, modifiedProgram) ->
            case checkMethodExtraction originalProgram modifiedProgram fromClass fromMethod toClass toMethod of
                Nothing -> putStrLn "refactoring verified successfully"
                Just (actual, expected) -> do
                    putStrLn $ "actual: " ++ show actual
                    putStrLn $ "expected: " ++ show expected
        Left err -> print err
