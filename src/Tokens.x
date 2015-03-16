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
  "//".*                            ;   -- Single-line comments
  "/*" ("/" | "*"* $commentable)*       -- Multi-line comments
        "*"+ "/"                    ;
  $digit+                           { \s -> TokenNum (read s) }
  "->"                              { \s -> TokenArrow }
  \=                                { \s -> TokenEq }
  \+                                { \s -> TokenPlus }
  \-                                { \s -> TokenMinus }
  \*                                { \s -> TokenMult }
  \:                                { \s -> TokenColon }
  \,                                { \s -> TokenComma }
  \(                                { \s -> TokenLParen }
  \)                                { \s -> TokenRParen }
  \{                                { \s -> TokenLBrack }
  \}                                { \s -> TokenRBrack }
  "case"                            { \s ->TokenCase }
  "otherwise"                       { \s ->TokenOtherwise }
  $alpha [$alpha $digit \_ \']*     { \s -> TokenSym s }

{

data Token = TokenNum Int
           | TokenSym String
           | TokenArrow
           | TokenEq
           | TokenPlus
           | TokenMinus
           | TokenMult
           | TokenColon
           | TokenComma
           | TokenLParen
           | TokenRParen
           | TokenLBrack
           | TokenRBrack
           | TokenCase
           | TokenOtherwise
           deriving (Eq,Show)

scanTokens = alexScanTokens

}

