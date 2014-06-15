{
module Lexer where

import Type
}

%wrapper "monad"

$digit      = [0-9]
$alpha      = [a-zA-Z_]
$alphanum   = [a-zA-Z0-9_]

@operator = "||" | "&&" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "+" | "-" | "*" | "/" | "%" | "!" | "--" | "++" |
    "(" | ")" | "{" | "}" | "," | ";" | "." | "="

@keyword = class | void | if | else | while | for | break | continue | return | null | this |
    new | boolean | byte | short | int | long | float | double

tokens :-

    $white+                     ;
    $digit+                     { makeToken (\s -> TLiteral . LInt . read $ s) }
    $digit+ [Ll]                { makeToken (\s -> TLiteral . LLong . read . init $ s) }
    $digit+ \. $digit+          { makeToken (\s -> TLiteral . LDouble .read $ s) }
    $digit+ \. $digit+ [Dd]?    { makeToken (\s -> TLiteral . LDouble .read . init  $ s) }
    $digit+ \. $digit+ [Ff]?    { makeToken (\s -> TLiteral . LFloat .read . init  $ s) }
    true                        { makeToken (\s -> TLiteral . LBoolean $ True) }
    false                       { makeToken (\s -> TLiteral . LBoolean $ False) }
    @operator                   { makeToken (\s -> TOperator s) }
    @keyword                    { makeToken (\s -> TKeyword s) }
    $alpha $alphanum*           { makeToken (\s -> TIdentifier s) }

{
alexEOF :: Alex Token
alexEOF = return TEOF

makeToken :: (String -> Token) -> AlexAction Token
makeToken f = token f'
    where f' (_, _, _, s) len = f . take len $ s

lexWrap :: (Token -> Alex a) -> Alex a
lexWrap cont = do
    tok <- alexMonadScan
    cont tok

getPos :: AlexPosn -> (Int, Int)
getPos (AlexPn _ line column) = (line, column)

infixl 4 <$>
(<$>) :: (a -> b) -> Alex a -> Alex b
f <$> a = do
    v <- a
    return $ f v

getLineNumber :: Alex Int
getLineNumber = fst <$> getPosition

getPosition :: Alex (Int, Int)
getPosition = Alex $ \s -> Right (s, getPos . alex_pos $ s)

data Token
    = TKeyword String
    | TIdentifier String
    | TOperator String
    | TLiteral Literal
    | TEOF
    deriving (Eq, Show)
}
