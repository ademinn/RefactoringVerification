{
module Parser where
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    class       { Token (TKeyword "class") _    }
    void        { Token (TKeyword "void") _     }
    if          { Token (TKeyword "if") _       }
    else        { Token (TKeyword "else") _     }
    while       { Token (TKeyword "while") _    }
    for         { Token (TKeyword "for") _      }
    break       { Token (TKeyword "break") _    }
    continue    { Token (TKeyword "continue") _ }
    return      { Token (TKeyword "return") _   }
    null        { Token (TKeyword "null") _     }
    this        { Token (TKeyword "this") _     }
    new         { Token (TKeyword "new") _      }
    boolean     { Token (TKeyword "boolean") _  }
    byte        { Token (TKeyword "byte") _     }
    short       { Token (TKeyword "short") _    }
    int         { Token (TKeyword "int") _      }
    long        { Token (TKeyword "long") _     }
    float       { Token (TKeyword "float") _    }
    double      { Token (TKeyword "double") _   }

    "||"        { Token (TOperator "||") _ }
    "&&"        { Token (TOperator "&&") _ }
    "=="        { Token (TOperator "==") _ }
    "!="        { Token (TOperator "!=") _ }
    "<"         { Token (TOperator "<") _ }
    ">"         { Token (TOperator ">") _ }
    "<="        { Token (TOperator "<=") _ }
    ">="        { Token (TOperator ">=") _ }
    "+"         { Token (TOperator "+") _ }
    "-"         { Token (TOperator "-") _ }
    "*"         { Token (TOperator "*") _ }
    "/"         { Token (TOperator "/") _ }
    "%"         { Token (TOperator "%") _ }
    "!"         { Token (TOperator "!") _ }
    "--"        { Token (TOperator "--") _ }
    "++"        { Token (TOperator "++") _ }
    "("         { Token (TOperator "(") _ }
    ")"         { Token (TOperator ")") _ }
    "{"         { Token (TOperator "{") _ }
    "}"         { Token (TOperator "}") _ }
    ","         { Token (TOperator ",") _ }
    ";"         { Token (TOperator ";") _ }
    "."         { Token (TOperator ".") _ }
    "="         { Token (TOperator "=") _ }

    literal     { Token (TLiteral $$) _ }

    identifier  { Token (TIdentifier $$) _ }

%nonassoc "<" ">"
%left "+" "-"
%left "*"

%%

Start
    : ClassList { $1 }

ClassList
    : ClassListRev  { reverse $1 }

ClassListRev
    : ClassListRev Class     { $2 : $1 }
    | {- empty -}         { [] }

Class
    : class identifier "{" MemberList "}"       { Class $2 $4 }

MemberList
    : MemberListRev     { reverse $1 }

MemberListRev
    : MemberListRev Member         { $2 : $1 }
    | {- empty -}           { [] }

Member
    : VariableDeclaration       { ??? $1 }
    | MethodDeclaration         { ??? $1 }

VariableDeclaration
    : Type identifier ";"       { Variable $1 $2 Nothing }
    | Type identifier "=" Expression ";"    { Variable $1 $2 (Just $4) }

MethodDeclaration
    : MaybeType identifier "(" ParameterList ")" Block      { Method $1 $2 $4 $6 }

MaybeType
    : Type      { ReturnType Type }
    | void      { Void }
    | {- empty -}   { Constructor }

ParameterList
    : ParameterListRev      { reverse $1 }

ParameterListRev
    : ParameterListRev "," Parameter    { $2 : $1 }
    | Parameter     { [$1] }

Parameter
    : Type identifier   { Parameter $1 $2 }

Block
    : "{" BlockStatementList "}"    { $2 }

BlockStatementList
    : BlockStatementListRev     { reverse $1 }

BlockStatementListRev
    : BlockStatementListRev BlockStatement  { $2 : $1 }
    | {- empty -}       { [] }

BlockStatement
    : VariableDeclaration   { ??? $1 }
    | Statement         { ??? $1 }

Statement
    : Block     { ??? $1 }
    | IfStatement   { ??? $1 }
    | WhileStatement    { ??? $1 }
    | ForStatement      { ??? $1 }
    | ReturnStatement   { ??? $1 }
    | break ";"            { Break }
    | continue ";"          { Continue }
    | Expression ";"        { ??? $1 }

IfStatement
    : if ParExperssion Statement ElseStatement      { If $2 $3 $4 }

ElseStatement
    : else Statement    { Just $2 }
    | {- empty -}       { Nothing }

WhileStatement
    : while ParExperssion Statement     { While $2 $3 }

ParExperssion
    : "(" Expression ")"    { $2 }

ForStatement
    : for "(" ForInit ForExpression ForIncrement ")" Statement      { For $3 $4 $5 $7 }

ForInit
    : VariableDeclaration   { ??? }
    | ExpressionList ";"    { ??? }

ForExpression
    : Expression ";"        { $1 }

ForIncrement
    : ExpressionList    { $1 }

ReturnStatement
    : return Expression ";"     { Just $2 }
    | return ";"        { Nothing }



{
parseError :: [Token] -> a
           parseError ((Token t (line :!: column)) : _) = error $ "unexpected token " ++ (show t) ++ " at position " ++ (show line) ++ ":" ++ (show column)
parseError [] = error "unexpected end of file"
}
