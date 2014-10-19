module Main where

import qualified System.Environment as Env

import qualified Lexer as L

import Parser
import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Monad.Error

codeExtension :: String
codeExtension = ".java"

assemblyExtension :: String
assemblyExtension = ".ll"

liftError :: ErrorT String IO a -> IO a
liftError = runErrorT >=> either fail return

splitLast :: Int -> String -> (String, String)
splitLast i s = splitAt (length s - i) s

main :: IO ()
main = do
    s <- getLine
    print $ L.runAlex s parse
