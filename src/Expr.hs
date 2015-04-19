module Expr where
import Types

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

define :: Id -> Type -> Expr -> Expr
-- Coerce Int to Pitch
define i (Data "Pitch") (VInt p)
    | p < 0 || p > 127  = error "Pitch out of range [0, 127]"
    | otherwise = Def i (Data "Pitch") (VPitch (Pitch p))
-- Coerce Int to Duration
define i (Data "Duration") (VInt d)
    | d < 1 || d > 256  = error "Duration out of range [1, 256]"
    | otherwise = Def i (Data "Duration") (VDuration (Duration d))
-- Otherwise
define i t e           = Def i t e

unpackList :: Expr -> [Expr]
unpackList (VList exprs) = exprs
unpackList _ = error "Syntax error"

unpackPitch :: Expr -> Pitch
unpackPitch (VPitch p) = p
unpackPitch _ = error "Syntax error"

construct :: Type -> [Expr] -> Expr
construct (Data "Pitch") [VInt p] = VPitch (Pitch p)
construct (Data "Note") [VPitch p, VDuration d]
    = VNote p d
construct (Data "Chord") [pitches, VDuration dur]
    = VChord (map unpackPitch (unpackList pitches)) dur
construct _ _ = error "Syntax error"

