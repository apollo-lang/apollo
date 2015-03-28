module Expr where

data Program
    = Program [Def]
    deriving Show

data Def
    = Def Decl Expr
    deriving Show

data Decl
    = Decl Id Type
    deriving Show

type Id = String

data Type
    = Data Id
    | Function [Decl] Type
    deriving Show

data Expr
    = Atom Primitive
    | Name Id
    | Block [Def] Expr
    | Cond Expr Expr Expr
    | Unary UnOp
    | Binary BinOp
    deriving Show

data Primitive
    = ApolloInt Int
    | ApolloBool Bool
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

