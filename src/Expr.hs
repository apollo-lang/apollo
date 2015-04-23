module Expr where

data Param
  = Param Id Type
  deriving Show

type Id = String

data Type
  = TData String
  | TList String
  | TFunc [Param] Type
  deriving Show

data Expr
  = VInt Int
  | VBool Bool
  | VDuration Duration
  | VPitch Pitch
  | VNote Pitch Duration
  | VChord [Pitch] Duration
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
  deriving Show

data IOpr = Add | Sub | Mul | Div | Mod
  deriving Show

data BOpr = Eq | NEq | Le | Gr | LEq | GEq | And | Or
  deriving Show

data Pitch = Pitch Int deriving Show
data Duration = Duration Int deriving Show

showVal :: Expr -> String
showVal (VInt  i) = show i
showVal (VBool b) = show b
showVal (VList l) = "[" ++ commaDelim l  ++ "]"
  where commaDelim = init . concatMap ((++ ",") . showVal)
showVal otherVal       = show otherVal

typeOf :: Expr -> String
typeOf (VInt _)  = "Integer"
typeOf (VBool _) = "Boolean"
typeOf (VList _) = "List"
typeOf todoVal        = "TODO: `typeOf` for " ++ show todoVal

