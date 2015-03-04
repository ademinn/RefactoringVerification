module FJ
( parse
, module FJ.AST
) where

import qualified FJ.Lexer as L
import qualified FJ.Parser as P
import FJ.Semantic
import FJ.AST

import Control.Monad.Except

parse :: String -> Either String Program
parse s = L.runAlex s P.parse >>= runExcept . checkProgram
