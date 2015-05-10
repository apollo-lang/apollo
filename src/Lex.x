{
{-# OPTIONS_GHC -w #-}

module Lex (
  Token(..)
  , scanTokens
) where

import Expr
import Type
}

%wrapper "basic"

$digit          = 0-9
$alpha          = [a-zA-Z]

tokens :-

    $white+                         ;

    -- Single-line comments
    "--".*                          ;

    -- Multi-line comments
    \{\- ([^\-] | [\r\n] | (\-+ ([^ \- \}] | [\r\n])))* \-+ \} ;

    -- Duration literal
    \\[0-9]+\.?                     { \s -> TokenDur s }

    -- Pitch literal
    [A-G](\#|b)?[0-9]               { \s -> TokenPitch s }

    -- Integer Constants
    $digit+                         { \s -> TokenNum (read s) }

    -- Boolean Constants
    "True"                          { \s -> TokenBool (True) }
    "False"                         { \s -> TokenBool (False) }

    -- Reserved words
    "case"                          { \s -> TokenCase }
    "otherwise"                     { \s -> TokenOtherwise }
    "where"                         { \s -> TokenWhere }


    -- Markers
    "#tempo"                        { \s -> TokenTempo }

    -- Identifiers
    [a-z][$alpha $digit \']*        { \s -> TokenId s }

    -- Type / Type Instance
    "Int"                           { \s -> TokenType TInt }
    "Bool"                          { \s -> TokenType TBool }
    "Duration"                      { \s -> TokenType TDuration }
    "Pitch"                         { \s -> TokenType TPitch }
    "Atom"                          { \s -> TokenType TAtom }
    "Music"                         { \s -> TokenType TMusic }

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
    "::"                            { \s -> TokenCons }
    \\                              { \s -> TokenLambda }
    "h@"                            { \s -> TokenHead }
    "t@"                            { \s -> TokenTail }

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
    \_                              { \s -> TokenUScore }

{

data Token = TokenId String
           | TokenNum Int
           | TokenBool Bool
           | TokenType Type
           | TokenDur String
           | TokenPitch String
           | TokenCase
           | TokenOtherwise
           | TokenWhere
           | TokenTempo
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
           | TokenCons
           | TokenLambda
           | TokenHead
           | TokenTail
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
           | TokenUScore
           deriving (Eq,Show)

scanTokens = alexScanTokens

}

