{
{-# OPTIONS_GHC -w #-}
module Lex
    ( Token(..)
    , scanTokens
    ) where

import Expr
}

%wrapper "basic"

$digit          = 0-9
$alpha          = [a-zA-Z]
$eol            = [\n]
$commentable    = [. \n] # [\/ \*]

tokens :-

    --^$eol\$?                      ;
    --$eol                          { \s -> TokenEOL }     
    $white+                         ;

    -- Single-line comments
    "--".*                          ;

    -- Multi-line comments
    "{-" 
    ("{"|"-"*$commentable)* 
    "-"+ "}"                        ;

    -- Duration literal
    \\[0-9]+\.?                     { \s -> TokenDur s }

    -- Pitch literal
    [A-G](\#|b)?[0-9]               { \s -> TokenPitch s }

    -- Rest literal (single quote + duration literal)
    '\\[0-9]+\.?                    { \s -> TokenRest s }

    -- Integer Constants
    $digit+                         { \s -> TokenNum (read s) }

    -- Boolean Constants
    "True"                          { \s -> TokenBool (True) }
    "False"                         { \s -> TokenBool (False) }

    -- Reserved words
    "case"                          { \s -> TokenCase }
    "otherwise"                     { \s -> TokenOtherwise }
    "where"                         { \s -> TokenWhere }

    -- Identifiers
    [a-z][$alpha $digit \']*        { \s -> TokenId s }

    -- Type / Type Instance
    [A-Z]$alpha*                    { \s -> TokenType s }

    -- Operators
    \+                              { \s -> TokenPlus }
    \-                              { \s -> TokenMinus }
    \*                              { \s -> TokenMult }
    \/                              { \s -> TokenDiv }
    \%                              { \s -> TokenMod }
    "=="                            { \s -> TokenEq }
    "!="                            { \s -> TokenNEq }
    \<                              { \s -> TokenLe }
    \>                              { \s -> TokenGr }
    "<="                            { \s -> TokenLEq }
    ">="                            { \s -> TokenGEq }
    "&&"                            { \s -> TokenAnd }
    "||"                            { \s -> TokenOr }
    \!                              { \s -> TokenNot }

    -- Separators
    \=                              { \s -> TokenDef }
    "->"                            { \s -> TokenArrow }
    \:                              { \s -> TokenColon }
    \,                              { \s -> TokenComma }
    \(                              { \s -> TokenLParen }
    \)                              { \s -> TokenRParen }
    \[                              { \s -> TokenLBrack }
    \]                              { \s -> TokenRBrack }
    \{                              { \s -> TokenLBrace }
    \}                              { \s -> TokenRBrace }

{

data Token = TokenId String
           | TokenNum Int
           | TokenBool Bool
           | TokenType String
           | TokenDur String
           | TokenPitch String
           | TokenRest String
           | TokenCase
           | TokenOtherwise
           | TokenWhere
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
           | TokenEOL
           deriving (Eq,Show)

scanTokens = alexScanTokens

}

