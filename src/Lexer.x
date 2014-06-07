{
module Lexer where

import Data.Strict.Tuple
import Type
}

%wrapper "posn"

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
data Token
    = Token
    { tokenType :: TokenType
    , position :: Pair Int Int
    }
    deriving (Eq, Show)

getPos :: AlexPosn -> Pair Int Int
getPos (AlexPn _ line column) = line :!: column

makeToken :: (String -> TokenType) -> AlexPosn -> String -> Token
makeToken f p s = Token (f s) (getPos p)

data TokenType
    = TKeyword String
    | TIdentifier String
    | TOperator String
    | TLiteral Literal
    deriving (Eq, Show)
}
