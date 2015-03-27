{
{-# OPTIONS_GHC -w #-}
module Tokens (Token(..),scanTokens) where
import Expr
}

%wrapper "basic"

$digit          = 0-9
$alpha          = [a-zA-Z]
$eol            = [\n]
$commentable    = [. \n] # [\/ \*]

tokens :-

  $eol                              ;
  $white+                           ;
  "--".*                            ;   -- Single-line comments
  "{-" ("{" | "-"* $commentable)*       -- Multi-line comments
        "-"+ "}"                    ;
  $digit+                           { \s -> TokenNum (read s) }
  "->"                              { \s -> TokenArrow }
  \=                                { \s -> TokenEq }
  \+                                { \s -> TokenPlus }
  \-                                { \s -> TokenMinus }
  \*                                { \s -> TokenMult }
  \%                                { \s -> TokenMod }
  \:                                { \s -> TokenColon }
  \,                                { \s -> TokenComma }
  \(                                { \s -> TokenLParen }
  \)                                { \s -> TokenRParen }
  \{                                { \s -> TokenLBrack }
  \}                                { \s -> TokenRBrack }
  "case"                            { \s ->TokenCase }
  "otherwise"                       { \s ->TokenOtherwise }
  "True"                            { \s ->TokenTrue }
  "False"                           { \s ->TokenFalse }
  $alpha [$alpha $digit \_ \']*     { \s -> TokenId s }

{

data Token = TokenNum Int
           | TokenId String
           | TokenArrow
           | TokenEq
           | TokenPlus
           | TokenMinus
           | TokenMult
           | TokenMod
           | TokenColon
           | TokenComma
           | TokenLParen
           | TokenRParen
           | TokenLBrack
           | TokenRBrack
           | TokenCase
           | TokenOtherwise
           | TokenTrue
           | TokenFalse
           deriving (Eq,Show)

scanTokens = alexScanTokens

}

