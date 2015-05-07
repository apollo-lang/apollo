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
    , Rest(..)
    , Note(..)
    , Chord(..)
    , Part(..)
    , Atom(..)
    , Music(..)
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
    | VRest Rest
    | VNote Note
    | VChord Chord
    | VPart [Expr]
    | VMusic [Expr]
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
data Atom     = AtomNote Note 
              | AtomChord Chord
              | AtomRest Rest          deriving (Eq, Ord, Show)
data Part     = Part [Atom]            deriving (Eq, Ord, Show)
data Music    = Music [Part]           deriving (Eq, Ord, Show)


commaDelim :: [Expr] -> String
commaDelim = init . concatMap ((++ ",") . showVal)

showVal :: Expr -> String
showVal (VInt  i)       = show i
showVal (VBool b)       = show b
showVal (VList l)       = "[" ++ commaDelim l  ++ "]"
showVal (VDuration d)   = show d
showVal (VPitch p)      = show p
showVal (VRest r)       = show r
showVal (VNote n)       = show n
showVal (VChord c)      = show c
showVal (VPart p)       = "Part {" ++ commaDelim p ++ "}"
showVal (VMusic m)      = "Music [" ++ commaDelim m ++ "]"
showVal (Empty)         = ""
showVal otherVal        = show otherVal

typeOf :: Expr -> String
typeOf VInt{}      = "Integer"
typeOf VBool{}     = "Boolean"
typeOf VDuration{} = "Duration"
typeOf VPitch{}    = "Pitch"
typeOf VRest{}     = "Rest"
typeOf VNote{}     = "Note"
typeOf VChord{}    = "Chord"
typeOf VPart{}     = "Part"
typeOf VMusic{}    = "Music"
typeOf VList{}     = "List"
typeOf Block{}     = "Block"
typeOf If{}        = "Conditional"
typeOf Def{}       = error "Error: typeOf called on Def"
typeOf Name{}      = error "Error: typeOf called on Name"
typeOf FnCall{}    = error "Error: typeOf called on FnCall"
typeOf Neg{}       = error "Error: typeOf called on Neg"
typeOf Not{}       = error "Error: typeOf called on Not"
typeOf IntOp{}     = error "Error: typeOf called on IntOp"
typeOf BoolOp{}    = error "Error: typeOf called on BoolOp"
typeOf CompOp{}    = error "Error: typeOf called on CompOp"
typeOf Empty       = error "Error: typeOf called on Empty"

