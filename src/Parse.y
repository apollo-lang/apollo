{
module Parse
( parse
, parseRepl
) where
import Control.Monad.Error (liftM)
import Error
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
    '['         { TokenLBrack }
    ']'         { TokenRBrack }
    '{'         { TokenLBrace }
    '}'         { TokenRBrace }

%nonassoc '='
%left '||'
%left '&&'
%left '==' '!=' '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/' '%'
%right NEG '!'

%%

Statements  : Statement                     { [$1] }
            | Statement Statements          { $1:$2 }

Statement   : Definition                    { $1 }
            | Expression                    { $1 }

Definition  : ID ':' Type '=' Expression    { define $1 $3 $5 }

Type        : TYPE                          { TData $1 }
            | '[' TYPE ']'                  { TList $2 }
            | FnType                        { $1 }

FnType      : '(' Params ')' '->' Type      { TFunc $2 $5 }

Param       : ID ':' Type                   { Param $1 $3 }

Params      : Param                         { [$1] }
            | Param ',' Params              { $1:$3 }

Expressions : Expression                    { [$1] }
            | Expression ',' Expressions    { $1:$3 }

Expression  : NUM                           { VInt $1 }
            | BOOL                          { VBool $1 }
            | ID                            { Name $1 }
            | DUR                           { VDuration $ parseDuration $1 }
            | PITCH                         { VPitch $ parsePitch $1 }
            | TYPE '(' Expressions ')'      { construct (TData $1) $3 }
            | '(' Expressions ')'           { construct (TData "Note") $2 } -- note syntax sugar
            | '{' Expressions '}'           { construct (TData "Chord") $2 } -- chord syntax sugar
            | ID '(' Expressions ')'        { FnCall $1 $3 }
            | '[' Expressions ']'           { VList $2 }
            | Conditional                   { $1 }
            | UnOp                          { $1 }
            | BinOp                         { $1 }
            | Block                         { $1 }
            | '(' Expression ')'            { $2 }

Conditional : CASE '(' Expression ')'
                Expression
              OTHERWISE
                Expression                  { If $3 $5 $7 }
            | CASE '(' Expression ')'
                Expression
              Conditional                   { If $3 $5 $6 }

UnOp        : '-' Expression  %prec NEG     { Neg $2 }
            | '!' Expression                { Not $2 }

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

Block       : '{' Expression '}'            { Block [] $2 }
            | '{' Statements Expression '}' { Block $2 $3 }

{
-- TODO: improve
parseError (token:whatever) = Left . ParseErr $ show whatever

parse :: String -> ThrowsError [Expr]
parse = program . scanTokens

-- TODO: implement single expr constraint at parse level
parseRepl :: String -> ThrowsError Expr
parseRepl = liftM head . parse
}
