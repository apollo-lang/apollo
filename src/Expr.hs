module Expr where

data Program
    = Program [Stmt]
    deriving Show

data Stmt
    = StDef Def
    | StExp Expr
    deriving Show

data Def
    = Def Id Type Expr
    deriving Show

data Param
    = Param Id Type
    deriving Show

type Id = String

data Type
    = Data Id
    | Function [Param] Type
    deriving Show

data Expr
    = ApolloInt Int
    | ApolloBool Bool
    | Name Id
    | Block [Def] Expr
    | Cond Expr Expr Expr
    | Unary UnOp
    | Binary BinOp
    deriving Show

data UnOp
    = Neg Expr
    | Not Expr
    deriving Show

data BinOp
    = Add Expr Expr     -- +
    | Sub Expr Expr     -- -
    | Mul Expr Expr     -- *
    | Div Expr Expr     -- /
    | Mod Expr Expr     -- %
    | Eq Expr Expr      -- ==
    | NEq Expr Expr     -- !=
    | Le Expr Expr      -- <
    | Gr Expr Expr      -- >
    | LEq Expr Expr     -- <=
    | GEq Expr Expr     -- >=
    | And Expr Expr     -- &&
    | Or Expr Expr      -- ||
    deriving Show

