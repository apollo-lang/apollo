module Expr
    ( Param(..)
    , Id
    , Type(..)
    , Expr(..)
    , IOpr(..)
    , BOpr(..)
    , COpr(..)
    , AOpr(..)
    , Pitch(..)
    , Duration(..)
    , Rest(..)
    , Note(..)
    , Chord(..)
    , Part(..)
    , Atom(..)
    , Music(..)
    , showVal
    ) where

data Type
    = TInt
    | TBool
    | TDuration
    | TPitch
    | TAtom
    | TPart
    | TMusic
    | TList Type
    | TEmpty String   -- TODO: remove
    | TError          -- TODO: remove
    | TFunc [Param] Type
    deriving (Eq, Ord)

instance Show Type where
    show TInt      = "Integer"
    show TBool     = "Boolean"
    show TDuration = "Duration"
    show TPitch    = "Pitch"
    show TAtom     = "Atom"
    show TPart     = "Part"
    show TMusic    = "Music"
    show (TList t) = "[" ++ show t ++ "]"
    show TEmpty{}  = "shouldnt show for TEmpty" -- TODO: remove
    show TError    = "shouldnt show for TEmpty" -- TODO: remove
    show TFunc{}   = "TODO show for TFunc"      -- TODO

data Param = Param Id Type
    deriving (Eq, Ord, Show)

type Id = String

-- TODO: note that FnBody stores untyped param names (just Ids)

data Expr
    = VInt Int
    | VBool Bool
    | VDuration Duration
    | VPitch Pitch
    | VAtom Expr Expr
    | VPart [Expr]
    | VMusic [Expr]
    | VList [Expr]
    | Def Id Type Expr
    | Name Id
    | Block [Expr] Expr
    | If Expr Expr Expr
    | FnCall Id [Expr]
    | FnBody [Id] Expr
    | Neg Expr
    | Not Expr
    | Head Expr
    | Tail Expr 
    | IntOp IOpr Expr Expr
    | BoolOp BOpr Expr Expr
    | CompOp COpr Expr Expr
    | ArrOp AOpr Expr Expr
    | Empty                     -- Value of definitions
    | Nil                       -- Value of '_' token
    deriving (Eq, Ord, Show)

data IOpr = Add | Sub | Mul | Div | Mod
    deriving (Eq, Ord)

instance Show IOpr where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Mod = "%"

data AOpr = Cons
    deriving (Eq, Ord)

instance Show AOpr where
    show Cons = "::"

data BOpr = And | Or
    deriving (Eq, Ord)

instance Show BOpr where
    show And = "&&"
    show Or  = "||"

data COpr = Eq | NEq | Le | Gr | LEq | GEq
    deriving (Eq, Ord)

instance Show COpr where
    show Eq  = "=="
    show NEq = "!="
    show Le  = "<"
    show Gr  = ">"
    show LEq = "<="
    show GEq = ">="

data Pitch    = Pitch Int              deriving (Eq, Ord, Show)
data Duration = Duration Int           deriving (Eq, Ord, Show)
data Note     = Note Pitch Duration    deriving (Eq, Ord, Show)
data Chord    = Chord [Pitch] Duration deriving (Eq, Ord, Show)
data Rest     = Rest Duration          deriving (Eq, Ord, Show)
data Atom     = AtomNote Note
              | AtomChord Chord
              | AtomRest Rest          deriving (Eq, Ord, Show)
data Part     = Part [Atom]            deriving (Eq, Ord, Show)
data Music    = Music [Part]           deriving (Eq, Ord, Show)

commaDelim :: [Expr] -> String
commaDelim [] = ""
commaDelim xs = init . concatMap ((++ ",") . showVal) $ xs

showVal :: Expr -> String
showVal (VInt  i)      = show i
showVal (VBool b)      = show b
showVal (VList l)      = "[" ++ commaDelim l  ++ "]"
showVal (VDuration d)  = show d
showVal (VPitch p)     = show p
showVal (VAtom p d)    = "Atom (" ++ showVal p ++ ", " ++ showVal d ++ ")"
showVal (VPart p)      = "Part {" ++ commaDelim p ++ "}"
showVal (VMusic m)     = "Music [" ++ commaDelim m ++ "]"
showVal (Empty)        = ""
showVal otherVal       = show otherVal

