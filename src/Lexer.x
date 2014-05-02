{
module Lexer where

import Data.Int
import Data.Strict.Tuple
}

%wrapper "posn"

$digit      = [0-9]

@idTail     = [A-Za-z0-9_]*

@operator = "||" | "&&" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "+" | "-" | "*" | "/" | "%" | "!" | "--" | "++" |
    "(" | ")" | "{" | "}" | "," | ";" | "."

@keyword = class | void | if | else | while | for | break | continue | return | null | this |
    new | boolean | byte | short | int | long | float | double

tokens :-

    $white+                 ;
    $digit+                 { makeToken (\s -> TLiteral . LInt . read $ s) }
    $digit+ [Ll]            { makeToken (\s -> TLiteral . LLong . read . init $ s) }
    [a-zA-Z_] [a-ZA-Z0-9_]* { makeToken (\s -> TId s) }
    @operator               { makeToken (\s -> TOp s) }
    @keyword                { makeToken (\s -> TKeyword s) }

{
data Token = Token TokenType (Pair Int Int)
    deriving (Eq, Show)

getPos :: AlexPosn -> Pair Int Int
getPos (AlexPn _ line column) = line :!: column

makeToken :: (String -> TokenType) -> AlexPosn -> String -> Token
makeToken f p s = Token (f s) (getPos p)

data Literal
    = LInt Int32
    | LLong Int64
    | LFloat Float
    | LDouble Double
    | LBoolean Bool
    deriving (Eq, Show)

data TokenType
    = TKeyword String
    | TId String
    | TOp String
    | TLiteral Literal
    deriving (Eq, Show)
}
