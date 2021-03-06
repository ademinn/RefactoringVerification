{
module FJ.Parser where

import FJ.Lexer
import FJ.ParseTree
import FJ.Common
import Data.Strict.Tuple
import Data.Either
import Data.Maybe
import Control.Monad.Identity
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
    extends     { TKeyword "extends"  }
    super       { TKeyword "super"    }

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
    : class identifier extends identifier "{" MemberList "}"       { buildClass $2 $4 $6 }

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
    : "{" super "(" SuperInitList ")" ";" AssignList "}"    { ConsBlock $4 $7 }

SuperInitList :: { [Identifier] }
SuperInitList
    : SuperInitListRev  { reverse $1 }
    | {- empty -}   { [] }

SuperInitListRev :: { [Identifier] }
SuperInitListRev
    : SuperInitListRev "," identifier   { $3 : $1 }
    | identifier    { [$1] }

AssignList :: { [Assign] }
AssignList
    : AssignListRev { reverse $1 }

AssignListRev :: { [Assign] }
AssignListRev
    : AssignListRev Assign { $2 : $1 }
    | {- empty -}   { [] }

Assign :: { Assign }
Assign
    : this "." identifier "=" identifier ";"    { Assign $3 $5 }

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

Expression :: { RawExpression }
Expression
    : new Type Expressions { Identity $ New $2 $3 }
    | Expression "." identifier  { Identity $ FieldAccess $1 $3 }
    | Expression "." identifier Expressions { Identity $ MethodCall $1 $3 $4 }
    | identifier { Identity $ Var $1 }
    | this { Identity $ Var "this" }

Expressions :: { [RawExpression] }
Expressions
    : "(" ExpressionList ")"    { $2 }

ExpressionList :: { [RawExpression] }
ExpressionList
    : ExpressionListRev { reverse $1 }
    | {- empty -}   { [] }

ExpressionListRev :: { [RawExpression] }
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

data Member where
    MV :: Variable -> Member
    MC :: Constructor -> Member
    MM :: Method -> Member

isVariable :: Member -> Maybe Variable
isVariable (MV v) = Just v
isVariable _ = Nothing

isConstructor :: Member -> Maybe Constructor
isConstructor (MC c) = Just c
isConstructor _ = Nothing

isMethod :: Member -> Maybe Method
isMethod (MM m) = Just m
isMethod _ = Nothing

mapVars :: [Member] -> [Variable]
mapVars = mapMaybe isVariable

mapCons :: [Member] -> [Constructor]
mapCons = mapMaybe isConstructor

mapMths :: [Member] -> [Method]
mapMths = mapMaybe isMethod

partitionMembers :: [Member] -> ([Variable], [Constructor], [Method])
partitionMembers members = (mapVars members, mapCons members, mapMths members)

buildClass :: String -> String -> [Member] -> Class
buildClass name nameBase members = Class name nameBase fields constructors methods
    where (fields, constructors, methods) = partitionMembers members

}
