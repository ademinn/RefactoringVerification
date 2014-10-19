{
module Parser where

import Lexer
import ParseTree
import Type
import Data.Strict.Tuple
import Data.Either
import Data.Maybe
}

%name parse
%tokentype { Token }
%error { parseError }
%lexer { lexWrap } { TEOF }
%monad { Alex }

%token
    class       { TKeyword "class"    }
    return      { TKeyword "return"   }
    this        { TKeyword "this"     }
    new         { TKeyword "new"      }

    "("         { TOperator "(" }
    ")"         { TOperator ")"  }
    "{"         { TOperator "{" }
    "}"         { TOperator "}" }
    ","         { TOperator "," }
    ";"         { TOperator ";" }
    "."         { TOperator "." }
    "="         { TOperator "=" }

    identifier  { TIdentifier $$ }

%right "="

%%

Start :: { [Class] }
Start
    : ClassList { $1 }

ClassList :: { [Class] }
ClassList
    : ClassListRev  { reverse $1 }

ClassListRev :: { [Class] }
ClassListRev
    : ClassListRev Class     { $2 : $1 }
    | {- empty -}         { [] }

Class :: { Class }
Class
    : class identifier "{" MemberList "}"       { buildClass $2 $4 }

MemberList :: { [Member] }
MemberList
    : MemberListRev     { reverse $1 }

MemberListRev :: { [Member] }
MemberListRev
    : MemberListRev Member         { $2 : $1 }
    | {- empty -}           { [] }

Member :: { Member }
Member
    : identifier MemberDefinition { $2 $1 }

MemberDefinition :: { String -> Member }
MemberDefinition
    : identifier Definition { \t -> $2 t $1 }
    | ConstructorDefinition { \cName -> MC $ $1 cName }

Definition :: { String -> String -> Member }
Definition
    : ";"   { \vType vName -> MV $ Variable vType vName }
    |  Parameters "{" return Expression ";" "}" { \mType mName -> MM $ Method mType mName $1 $4 }

ConstructorDefinition :: { String -> Constructor }
ConstructorDefinition
    : Parameters ConsBlock { \cName -> Constructor cName $1 $2 }

ConsBlock :: { ConsBlock }
ConsBlock
    : "{" AssignList "}"    { ConsBlock }

AssignList :: { () }
AssignList
    : AssignListRev { $1 }

AssignListRev :: { () }
AssignListRev
    : AssignListRev Assign { () }
    | {- empty -}   { () }

Assign :: { () }
Assign
    : this "." identifier "=" identifier ";"    { () }

Parameters :: { [Variable] }
Parameters
    : "(" ParameterList ")" { $2 }

ParameterList :: { [Variable] }
ParameterList
    : ParameterListRev      { reverse $1 }
    | {- empty -}   { [] }

ParameterListRev :: { [Variable] }
ParameterListRev
    : ParameterListRev "," Parameter    { $3 : $1 }
    | Parameter     { [$1] }

Parameter :: { Variable }
Parameter
    : Type identifier   { Variable $1 $2 }

Expression :: { Expression }
Expression
    : new Type Expressions { New $2 $3 }
    | Expression "." identifier  { FieldAccess $1 $3 }
    | Expression "." identifier Expressions { MethodCall $1 $3 $4 }
    | identifier { Var $1 }
    | this { This }

Expressions :: { [Expression] }
Expressions
    : "(" ExpressionList ")"    { $2 }

ExpressionList :: { [Expression] }
ExpressionList
    : ExpressionListRev { reverse $1 }
    | {- empty -}   { [] }

ExpressionListRev :: { [Expression] }
ExpressionListRev
    : ExpressionListRev "," Expression  { $3 : $1 }
    | Expression    { [$1] }

Type :: { Type }
Type
    : identifier { $1 }

{
parseError :: Token -> Alex a
parseError t = do
    (line, column) <- getPosition
    alexError $ "unexpected token " ++ show t ++ " at line " ++ show line ++ ", column " ++ show (column - 1)

withLine :: (Int -> a) -> Alex a
withLine f = f <$> getLineNumber

data Member = MV Variable | MC Constructor | MM Method

partitionMembers :: [Member] -> ([Variable], Constructor, [Method])
partitionMembers members = (mapMembers isVariable, head $ mapMembers isConstructor, mapMembers isMethod)
    where
        mapMembers f = mapMaybe f members

        isVariable (MV v) = Just v
        isVariable _ = Nothing

        isConstructor (MC c) = Just c
        isConstructor _ = Nothing
        
        isMethod (MM m) = Just m
        isMethod _ = Nothing

buildClass :: String -> [Member] -> Class
buildClass name members = Class name fields constructor methods
    where (fields, constructor, methods) = partitionMembers members

}
