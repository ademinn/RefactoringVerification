module FJ
( parse
) where

import qualified FJ.Lexer as L
import qualified FJ.Parser as P
import FJ.Semantic
import FJ.AST

import Control.Monad.Except

parse :: String -> Either String Program
parse s = do
    parseTree <- L.runAlex s P.parse
    runExcept . checkProgram $ parseTree
--    args <- getArgs
--    let originalFile = args !! 0
--        modifiedFile = args !! 1
--        fromClass = args !! 2
--        fromMethod = args !! 3
--        toClass = args !! 4
--        toMethod = args !! 5
--    original <- readFile originalFile
--    modified <- readFile modifiedFile
--    let originalRes = L.runAlex original parse
--        modifiedRes = L.runAlex modified parse
--    case (,) <$> originalRes <*> modifiedRes of
--        Right (originalPT, modifiedPT) -> do
--            let originalProgramRes = runExcept $ checkProgram originalPT
--                modifiedProgramRes = runExcept $ checkProgram modifiedPT
--            case (,) <$> originalProgramRes <*> modifiedProgramRes of
--                Right (originalProgram, modifiedProgram) ->
--                    case checkMethodExtraction originalProgram modifiedProgram fromClass fromMethod toClass toMethod of
--                        Nothing -> putStrLn "refactoring verified successfully"
--                        Just (actual, expected) -> do
--                            putStrLn $ "actual: " ++ show actual
--                            putStrLn $ "expected: " ++ show expected
--                Left err -> print err
--        Left err -> print err
