module Expr
( Param(..)
, Id
, Type(..)
, Expr(..)
, IOpr(..)
, BOpr(..)
, COpr(..)
, Pitch(..)
, Duration(..)
, Note(..)
, Chord(..)
, showVal
, typeOf
) where

data Type
  = TData String
  | TList String
  | TFunc [Param] Type
  deriving (Eq, Ord, Show)

data Param = Param Id Type
  deriving (Eq, Ord, Show)

type Id = String

data Expr
    = VInt Int
    | VBool Bool
    | VDuration Duration
    | VPitch Pitch
    | VNote Note
    | VChord Chord
    | VList [Expr]
    | Def Id Type Expr
    | Name Id
    | Block [Expr] Expr
    | If Expr Expr Expr
    | FnCall Id [Expr]
    | Neg Expr
    | Not Expr
    | IntOp IOpr Expr Expr
    | BoolOp BOpr Expr Expr
    | CompOp COpr Expr Expr
    | Empty
    deriving (Eq, Ord, Show)

data IOpr = Add | Sub | Mul | Div | Mod
  deriving (Eq, Ord, Show)

data BOpr = And | Or
  deriving (Eq, Ord, Show)

data COpr = Eq | NEq | Le | Gr | LEq | GEq
  deriving (Eq, Ord, Show)

data Pitch    = Pitch Int              deriving (Eq, Ord, Show)
data Duration = Duration Int           deriving (Eq, Ord, Show)
data Note     = Note Pitch Duration    deriving (Eq, Ord, Show)
data Chord    = Chord [Pitch] Duration deriving (Eq, Ord, Show)
data Rest     = Rest Duration          deriving (Eq, Ord, Show)

showVal :: Expr -> String
showVal (VInt  i) = show i
showVal (VBool b) = show b
showVal (VList l) = "[" ++ commaDelim l  ++ "]"
  where commaDelim = init . concatMap ((++ ",") . showVal)
showVal (Empty)   = ""
showVal otherVal  = show otherVal

typeOf :: Expr -> String
typeOf (VInt _)  = "Integer"
typeOf (VBool _) = "Boolean"
typeOf (VList _) = "List"
typeOf todoVal   = "TODO: `typeOf` for " ++ show todoVal

