module Expr where

data Program
    = Program [Expr]
    deriving Show

data Expr
    = Val Int
    | Assign Var Expr
    | Block [Expr]
    | Cond Expr Expr Expr
    deriving Show

data Var
    = Var Id VarType
    deriving Show

data VarType
    = DataType Id
    | FunctionType [Var] Id
    deriving Show

data BinOp
    = Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    deriving Show

eval :: BinOp -> Expr
eval (Add (Val a) (Val b)) = Val (a + b)

type Id = String

