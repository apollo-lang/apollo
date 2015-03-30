{
{-# OPTIONS_GHC -w #-}
module Lexer (Token(..),scanTokens) where
import Expr
}

%wrapper "basic"

$digit          = 0-9
$alpha          = [a-zA-Z]
$eol            = [\n]
$commentable    = [. \n] # [\/ \*]

tokens :-

    $eol                          ;
    $white+                       ;

    -- Single-line comments
    "--".*                        ;   

    -- Multi-line comments
    "{-" 
    ("{"|"-"*$commentable)* 
    "-"+ "}"                      ;

    -- Duration constant
    \\[0-9]+\.?                   { \s -> TokenDur s }

    -- Pitch constant
    `[A-G](\#|b)?[0-9]            { \s -> TokenPitch s }

    -- Integer Constants
    $digit+                       { \s -> TokenNum (read s) }

    -- Boolean Constants
    "True"                        { \s -> TokenBool (True) }
    "False"                       { \s -> TokenBool (False) }

    -- Reserved words
    "case"                        { \s -> TokenCase }
    "otherwise"                   { \s -> TokenOtherwise }

    -- Identifiers
    [a-z][$alpha $digit \']*      { \s -> TokenId s }

    -- Type / Type Instance
    [A-Z]$alpha*                  { \s -> TokenType s }

    -- Operators
    \+                            { \s -> TokenPlus }
    \-                            { \s -> TokenMinus }
    \*                            { \s -> TokenMult }
    \/                            { \s -> TokenDiv }
    \%                            { \s -> TokenMod }
    "=="                          { \s -> TokenEq }
    "!="                          { \s -> TokenNEq }
    \<                            { \s -> TokenLe }
    \>                            { \s -> TokenGr }
    "<="                          { \s -> TokenLEq }
    ">="                          { \s -> TokenGEq }
    "&&"                          { \s -> TokenAnd }
    "||"                          { \s -> TokenOr }
    \!                            { \s -> TokenNot }

    -- Separators
    \=                            { \s -> TokenDef }
    "->"                          { \s -> TokenArrow }
    \:                            { \s -> TokenColon }
    \,                            { \s -> TokenComma }
    \(                            { \s -> TokenLParen }
    \)                            { \s -> TokenRParen }
    \[                            { \s -> TokenLBrack }
    \]                            { \s -> TokenRBrack }
    \{                            { \s -> TokenLBrace }
    \}                            { \s -> TokenRBrace }

{

data Token = TokenId String
           | TokenNum Int
           | TokenBool Bool
           | TokenType String
           | TokenDur String
           | TokenPitch String
           | TokenCase
           | TokenOtherwise
           | TokenPlus
           | TokenMinus
           | TokenMult
           | TokenDiv
           | TokenMod
           | TokenEq
           | TokenNEq
           | TokenLe
           | TokenGr
           | TokenLEq
           | TokenGEq
           | TokenAnd
           | TokenOr
           | TokenNot
           | TokenDef
           | TokenArrow
           | TokenColon
           | TokenComma
           | TokenLParen
           | TokenRParen
           | TokenLBrack
           | TokenRBrack
           | TokenLBrace
           | TokenRBrace
           deriving (Eq,Show)

scanTokens = alexScanTokens

}

