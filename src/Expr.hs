module Expr where
import Types

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
    = Data String
    | ListT String
    | Function [Param] Type
    deriving Show

data Expr
    = ApolloInt Int
    | ApolloBool Bool
    | ApolloDuration Duration
    | ApolloPitch Pitch
    | ApolloNote Pitch Duration
    | ApolloChord [Pitch] Duration
    | ApolloList [Expr]
    | Name Id
    | Block [Stmt] Expr
    | Cond Expr Expr Expr
    | FnCall Id [Expr]
    -- Unary Operators
    | Neg Expr          -- -
    | Not Expr          -- !
    -- Binary Operators
    | Add Expr Expr     -- +
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

instance Show Expr where
  show (ApolloInt  i) = show i
  show (ApolloBool b) = show b
  show (ApolloList l) = show l
  show otherVal       = show otherVal

define :: Id -> Type -> Expr -> Def
-- Coerce Int to Pitch
define i (Data "Pitch") (ApolloInt p)
    | p < 0 || p > 127  = error "Pitch out of range [0, 127]"
    | otherwise = Def i (Data "Pitch") (ApolloPitch (Pitch p))
-- Coerce Int to Duration
define i (Data "Duration") (ApolloInt d)
    | d < 1 || d > 256  = error "Duration out of range [1, 256]"
    | otherwise = Def i (Data "Duration") (ApolloDuration (Duration d))
-- Otherwise
define i t e           = Def i t e

unpackList :: Expr -> [Expr]
unpackList (ApolloList exprs) = exprs
unpackList _ = error "Syntax error"

unpackPitch :: Expr -> Pitch
unpackPitch (ApolloPitch p) = p
unpackPitch _ = error "Syntax error"

construct :: Type -> [Expr] -> Expr
construct (Data "Pitch") [ApolloInt p] = ApolloPitch (Pitch p)
construct (Data "Note") [ApolloPitch p, ApolloDuration d]
    = ApolloNote p d
construct (Data "Chord") [pitches, ApolloDuration dur]
    = ApolloChord (map unpackPitch (unpackList pitches)) dur
construct _ _ = error "Syntax error"

