module TestLexer where

import Test.HUnit

import Lexer

checkToken :: String -> (TokenType -> Bool) -> Test.HUnit.Test
checkToken s f = TestLabel ("test token \"" ++ s ++ "\"") $ TestCase $ do
    let tokens = alexScanTokens s
    length tokens @?= 1
    let Token t _ = head tokens
    f t @? ""

checkTokenEq :: String -> TokenType -> Test.HUnit.Test
checkTokenEq s t = checkToken s (\t' -> t' == t)

testLexer :: Test.HUnit.Test
testLexer = TestList [
        checkTokenEq "123" (TLiteral . LInt $ 123),
        checkTokenEq "123L" (TLiteral . LLong $ 123)
    ]

