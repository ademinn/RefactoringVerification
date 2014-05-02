module TestLexer (testLexer) where

import Test.HUnit

import Lexer

checkToken :: String -> (TokenType -> Bool) -> Test
checkToken s f = TestLabel ("test token \"" ++ s ++ "\"") $ TestCase $ do
    let tokens = alexScanTokens s
    length tokens @?= 1
    f (tokenType . head $ tokens) @? ""

checkTokenList :: String -> [TokenType] -> Test
checkTokenList s ts = TestLabel ("test token list \"" ++ s ++ "\"") $ TestCase $ do
    let tokens = alexScanTokens s
    (map tokenType tokens) @?= ts

checkTokenEq :: String -> TokenType -> Test
checkTokenEq s t = checkToken s (\t' -> t' == t)

isFloat :: TokenType -> Bool
isFloat (TLiteral (LFloat _)) = True
isFloat _ = False

isDouble :: TokenType -> Bool
isDouble (TLiteral (LDouble _)) = True
isDouble _ = False

checkKeyword :: String -> Test
checkKeyword s = checkTokenEq s (TKeyword s)

checkOperator :: String -> Test
checkOperator s = checkTokenEq s (TOperator s)

checkIdentifier :: String -> Test
checkIdentifier s = checkTokenEq s (TIdentifier s)

testLexer :: Test.HUnit.Test
testLexer = TestList $ [
        checkTokenEq "123" (TLiteral . LInt $ 123),
        checkTokenEq "123L" (TLiteral . LLong $ 123),
        checkTokenEq "true" (TLiteral . LBoolean $ True),
        checkTokenEq "false" (TLiteral . LBoolean $ False),
        checkToken "123.123" isDouble,
        checkToken "123.123" isDouble,
        checkToken "123.123D" isDouble,
        checkToken "123.123F" isFloat,
        uncurry checkTokenList tokenList
    ] ++ (map checkKeyword keywords)
    ++ (map checkOperator operators)
    ++ (map checkIdentifier identifiers)

tokenList :: (String, [TokenType])
tokenList = ("class A { int x; A(int x) { this.x = x; } }",
    [
        TKeyword "class", TIdentifier "A", TOperator "{",
        TKeyword "int", TIdentifier "x", TOperator ";",
        TIdentifier "A", TOperator "(", TKeyword "int", TIdentifier "x", TOperator ")", TOperator "{",
        TKeyword "this", TOperator ".", TIdentifier "x", TOperator "=", TIdentifier "x", TOperator ";",
        TOperator "}", TOperator "}"
    ])

keywords :: [String]
keywords = [
        "class",
        "void",
        "if",
        "else",
        "while",
        "for",
        "break",
        "continue",
        "return",
        "null",
        "this",
        "new",
        "boolean",
        "byte",
        "short",
        "int",
        "long",
        "float",
        "double"
    ]

operators :: [String]
operators = [
        "||",
        "&&",
        "==",
        "!=",
        "<",
        ">",
        "<=",
        ">=",
        "+",
        "-",
        "*",
        "/",
        "%",
        "!",
        "--",
        "++",
        "(",
        ")",
        "{",
        "}",
        ",",
        ";",
        ".",
        "="
    ]

identifiers :: [String]
identifiers = [
        "class1",
        "value",
        "BufferedInputStream",
        "CONSTANT_NAME",
        "_strange_name",
        "a_1_b_"
    ]
