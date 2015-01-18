{
module FJ.Lexer where
}

%wrapper "monad"

$digit      = [0-9]
$alpha      = [a-zA-Z_]
$alphanum   = [a-zA-Z0-9_]

@operator = "(" | ")" | "{" | "}" | "," | ";" | "." | "="

@keyword = class | return | this | new | extends | super

tokens :-

    $white+                     ;
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
    | TEOF
    deriving (Eq, Show)
}
