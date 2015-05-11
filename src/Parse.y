{
module Parse (
  parse
) where

import Control.Monad.Error (liftM, throwError)
import Error
import Type
import Expr
import Util
import Lex
}

%name           program
%monad          { ThrowsError }
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
    WHERE       { TokenWhere }
    TEMPO       { TokenTempo }
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
    '::'        { TokenCons }
    '\\'        { TokenLambda }
    'h@'        { TokenHead }
    't@'        { TokenTail }
    '?'         { TokenRandom }
    '='         { TokenDef }
    '->'        { TokenArrow }
    ':'         { TokenColon }
    ','         { TokenComma }
    '('         { TokenLParen }
    ')'         { TokenRParen }
    '['         { TokenLBrack }
    ']'         { TokenRBrack }
    '{'         { TokenLBrace }
    '}'         { TokenRBrace }
    '_'         { TokenUScore }

%nonassoc '='
%left '||'
%left '&&'
%left '==' '!=' '<' '>' '<=' '>='
%right '::'
%left '+' '-'
%left '*' '/' '%'
%right NEG '!' 'h@' 't@'

%%

Program     : {- empty -}                   { [] }
            | Statements                    { $1 }

Statements  : Statement                     { [$1] }
            | Statement Statements          { $1:$2 }

Statement   : Definition                    { $1 }
            | Expression                    { $1 }

Definitions : Definition                    { [$1] }
            | Definition Definitions        { $1:$2 }

Definition  : ID ':' Type '=' Expression    { Def $1 $3 $5 }

            | ID ':'
              '(' Params ')' '->' Type
              '=' Expression                { def $1 $4 $7 $9 }

            | TEMPO Expression              { Def "#tempo" TInt $2}

Type        : TYPE                          { $1 }
            | '[' Type ']'                  { TList $2 }

Param       : ID ':' Type                   { Param $1 $3 }
            | ID ':'
              '(' AnonParams ')' '->'
              Type                          { param $1 $4 $7 }

Params      : Param                         { [$1] }
            | Param ',' Params              { $1:$3 }

AnonParam   : Type                          { Param "" $1 }

AnonParams  : AnonParam                     { [$1] }
            | AnonParam ',' AnonParams      { $1:$3 }

Expressions : Expression                    { [$1] }
            | Expression ',' Expressions    { $1:$3 }

Expression  : Primitive                     { $1 }
            | Derived                       { $1 }
            | Lambda                        { $1 }
            | FnCall                        { $1 }
            | Conditional                   { $1 }
            | Block                         { $1 }
            | UnOp                          { $1 }
            | BinOp                         { $1 }
            | '(' Expression ')'            { $2 }
            | ID                            { Name $1 }
            | Macro                         { $1 }

Primitive   : NUM                           { VInt $1 }
            | BOOL                          { VBool $1 }
            | PITCH                         { VPitch $ parsePitch $1 }
            | DUR                           { VDuration $ parseDuration $1 }
            | '_'                           { Nil }

Derived     : '(' Expression
              ',' Expression ')'            { VAtom $2 $4 }
            | '(' '_' ',' Expression ')'    { VAtom Nil $4 }
            | '[' Expressions ']'           { VList $2 }
            | '[' ']'                       { VList [] }

Lambda      : '\\' Params '->' Type ':'
              Expression                    { lambda $2 $4 $6 }

FnCall      :  ID '(' Expressions ')'       { FnCall (Name $1) $3 }
            | '(' Lambda ')'
              '(' Expressions ')'           { FnCall $2 $5 }

Conditional : CASE '(' Expression ')'
                Expression
              OTHERWISE
                Expression                  { If $3 $5 $7 }
            | CASE '(' Expression ')'
                Expression
              Conditional                   { If $3 $5 $6 }

UnOp        : '-' Expression  %prec NEG     { Neg $2 }
            | '!' Expression                { Not $2 }
            | 'h@' Expression               { Head $2 }
            | 't@' Expression               { Tail $2 }

BinOp       : Expression '+'  Expression    { IntOp  Add $1 $3 }
            | Expression '-'  Expression    { IntOp  Sub $1 $3 }
            | Expression '*'  Expression    { IntOp  Mul $1 $3 }
            | Expression '/'  Expression    { IntOp  Div $1 $3 }
            | Expression '%'  Expression    { IntOp  Mod $1 $3 }
            | Expression '==' Expression    { CompOp Eq  $1 $3 }
            | Expression '!=' Expression    { CompOp NEq $1 $3 }
            | Expression '<'  Expression    { CompOp Le  $1 $3 }
            | Expression '>'  Expression    { CompOp Gr  $1 $3 }
            | Expression '<=' Expression    { CompOp LEq $1 $3 }
            | Expression '>=' Expression    { CompOp GEq $1 $3 }
            | Expression '&&' Expression    { BoolOp And $1 $3 }
            | Expression '||' Expression    { BoolOp Or  $1 $3 }
            | Expression '::' Expression    { ArrOp Cons $1 $3 }

Macro       : NUM '?' NUM                   { VInt $ randomRange $1 $3 }

Block       : '{' Expression '}'            { Block [] $2 }
            | '{' Expression
              WHERE Definitions '}'         { Block $4 $2 }

{
parseError (t:ts) = throwError . ParseErr $ "token " ++ show t
parseError []     = throwError . ParseErr $ "end of input"

parse :: String -> ThrowsError [Expr]
parse = program . scanTokens
}
