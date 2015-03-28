{
module Parser where

import Lexer
import Expr
import Types
}

%name           program
%tokentype      { Token }
%error          { parseError }

%token
    ID          { TokenId $$ }
    NUM         { TokenNum $$ }
    BOOL        { TokenBool $$ }
    TYPE        { TokenType $$ }
    DUR         { TokenDur $$ }
    PITCH       { TokenPitch $$ }
    CASE        { TokenCase }
    OTHERWISE   { TokenOtherwise }
    '+'         { TokenPlus }
    '-'         { TokenMinus }
    '*'         { TokenMult }
    '/'         { TokenDiv }
    '%'         { TokenMod }
    '=='        { TokenEq }
    '!='        { TokenNEq }
    '<'         { TokenLe }
    '>'         { TokenGr }
    '<='        { TokenLEq }
    '>='        { TokenGEq }
    '&&'        { TokenAnd }
    '||'        { TokenOr }
    '!'         { TokenNot }
    '='         { TokenDef }
    '->'        { TokenArrow }
    ':'         { TokenColon } 
    ','         { TokenComma }
    '('         { TokenLParen }
    ')'         { TokenRParen }
    '{'         { TokenLBrack }
    '}'         { TokenRBrack }

%nonassoc '='
%left '||'
%left '&&'
%left '==' '!=' '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/' '%'
%right NEG '!'

%%

Program     : Statements                    { Program $1 }

Statements  : Statement                     { [$1] }
            | Statement Statements          { $1:$2 }

Statement   : Definition                    { StDef $1 }
            | Expression                    { StExp $1 }

Definitions : Definition                    { [$1] }
            | Definition Definitions        { $1:$2 }

Definition  : ID ':' Type '=' Expression    { Def $1 $3 $5 }

Type        : TYPE                          { Data $1 }
            | FnType                        { $1 }

FnType      : '(' Params ')' '->' Type      { Function $2 $5 }

Param       : ID ':' Type                   { Param $1 $3 }

Params      : Param                         { [$1] }
            | Param ',' Params              { $1:$3 }

Expression  : NUM                           { ApolloInt $1 }
            | BOOL                          { ApolloBool $1 }
            | ID                            { Name $1 }
            | DUR                           { ApolloDuration $ parseDuration $1 }
            | PITCH                         { ApolloPitch $ parsePitch $1 }
            | Conditional                   { $1 }
            | UnOp                          { Unary $1 }
            | BinOp                         { Binary $1 }
            | Block                         { $1 }
            | '(' Expression ')'            { $2 }

Conditional : CASE '(' Expression ')' 
                Expression 
              OTHERWISE 
                Expression                  { Cond $3 $5 $7 }
            | CASE '(' Expression ')' 
                Expression 
              Conditional                   { Cond $3 $5 $6 }

UnOp        : '-' Expression  %prec NEG     { Neg $2 }
            | '!' Expression                { Not $2 }

BinOp       : Expression '+' Expression     { Add $1 $3 }
            | Expression '-' Expression     { Sub $1 $3 }
            | Expression '*' Expression     { Mul $1 $3 }
            | Expression '/' Expression     { Div $1 $3 }
            | Expression '%' Expression     { Mod $1 $3 }
            | Expression '==' Expression    { Eq $1 $3 }
            | Expression '!=' Expression    { NEq $1 $3 }
            | Expression '<' Expression     { Le $1 $3 }
            | Expression '>' Expression     { Gr $1 $3 }
            | Expression '<=' Expression    { LEq $1 $3 }
            | Expression '>=' Expression    { GEq $1 $3 }
            | Expression '&&' Expression    { And $1 $3 }
            | Expression '||' Expression    { Or $1 $3 }

Block       : '{' Expression '}'            { Block [] $2 }
            | '{' Definitions Expression '}'{ Block $2 $3 }

{
parseError :: [Token] -> a
parseError = error "Parse error"

parseProgram :: String -> Program
parseProgram = program . scanTokens
}
