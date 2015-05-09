{
module Parse (
  parse
) where
import Control.Monad.Error (liftM, throwError)
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
    MUSIC       { TokenMusic }
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
    '|'         { TokenPipe }
    '_'         { TokenUScore }

%nonassoc '=' '->'
%left '||'
%left '&&'
%left '==' '!=' '<' '>' '<=' '>='
%right '::'
%left '+' '-'
%left '*' '/' '%'
%right NEG '!'

%%

Statements  : Statement                     { [$1] }
            | Statement Statements          { $1:$2 }

Statement   : Definition                    { $1 }
            | Expression                    { $1 }

Definitions : Definition                    { [$1] }
            | Definition Definitions        { $1:$2 }

Definition  : ID ':' Type '=' Expression    { Def $1 $3 $5 }
            | ID ':' 
              '(' Params ')' '->' Type 
              '=' Expression                { def $1 ($4, $7) $9 }
            | TEMPO Expression              { Def "#tempo" TInt $2}

Type        : TYPE                          { $1 }
            | '[' Type ']'                  { TList $2 }
            | MUSIC                         { TMusic }

FnType      : '(' AnonParams ')' '->' Type  { ($2, $5) }

Param       : ID ':' Type                   { Param $1 $3 }
            | ID ':' FnType                 { param $1 $3 }

Params      : Param                         { [$1] }
            | Param ',' Params              { $1:$3 }

AnonParam   : Type                          { Param "" $1 }

AnonParams  : AnonParam                     { [$1] }
            | AnonParam ',' AnonParams      { $1:$3 }

Expressions : Expression                    { [$1] }
            | Expression ',' Expressions    { $1:$3 }

Expression  : NUM                           { VInt $1 }
            | BOOL                          { VBool $1 }
            | ID                            { Name $1 }
            | PITCH                         { VPitch $ parsePitch $1 }
            | DUR                           { VDuration $ parseDuration $1 }
            | TEMPO                         { Name "#tempo" }
            | MUSIC '(' Expressions ')'     { VMusic $3 }
            | '(' Expression
              ',' Expression ')'            { VAtom $2 $4 }     -- Note and Chord atoms
            | '(' '_' ',' Expression ')'    { VAtom Nil $4 }    -- Rest atom
            | '|' Expressions '|'           { VPart $2 }
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
            | Expression '::' Expression    { ArrOp Cons $1 $3 }

Block       : '{' Expression '}'            { Block [] $2 }
            | '{' Expression
              WHERE Definitions '}'         { Block $4 $2 }

{
parseError (t:ts) = throwError . ParseErr $ "token " ++ show t
parseError []     = throwError . ParseErr $ "end of input"

parse :: String -> ThrowsError [Expr]
parse = program . scanTokens
}
