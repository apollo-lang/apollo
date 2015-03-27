{
module Parser where

import Tokens
import Expr
}

%name program
%tokentype      { Token }
%error          { parseError }

%token
    NUM         { TokenNum $$ }
    ID          { TokenId $$ }
    '->'        { TokenArrow }
    '='         { TokenEq }
    '+'         { TokenPlus }
    '-'         { TokenMinus }
    '*'         { TokenMult }
    '%'         { TokenMod }
    ':'         { TokenColon } 
    ','         { TokenComma }
    '('         { TokenLParen }
    ')'         { TokenRParen }
    '{'         { TokenLBrack }
    '}'         { TokenRBrack }
    CASE        { TokenCase }
    OTHERWISE   { TokenOtherwise }
    TRUE        { TokenTrue }
    FALSE       { TokenFalse }

%%

Program     : Expressions                   { Program $1 }

Expressions : Expression                    { [$1] }
            | Expression Expressions        { $1:$2 }

Expression  : Assignment                    { $1 }
            | Value                         { $1 }
            | Block                         { $1 }
            | Conditional                   { $1 }

Assignment  : Declaration '=' Expression    { Assign $1 $3 }

Declaration : ID ':' ID                     { Var $1 (DataType $3) }
            | ID ':' FnType                 { Var $1 $3 }

FnType      : '(' Params ')' '->' ID        { FunctionType $2 $5 }

Params      : Declaration                   { [$1] }
            | Declaration ',' Params        { $1:$3 }

Value       : NUM                           { Val $1 }
            | Boolean                       { $1 }

Boolean     : TRUE                          { True }
            | FALSE                         { False }

Block       : '{' Expressions '}'           { Block $2 }

Conditional : CASE '(' Expression ')'
                Expression
              OTHERWISE
                Expression                  { Cond $3 $5 $7 }
            | CASE '(' Expression ')' 
                Expression
              Conditional                   { Cond $3 $5 $6 }

{
parseError :: [Token] -> a
parseError = error "Parse error"

parseProgram :: String -> Program
parseProgram = program . scanTokens
}
