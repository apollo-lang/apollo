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
    ) where

data Type
  = TInt
  | TBool
  | TDuration
  | TPitch
  | TRest
  | TNote
  | TChord
  | TList Type

  | TEmpty String -- TODO: remove
  | TError -- TODO: remove

  | TData String
  | TFunc [Param] Type
  deriving (Eq, Ord)

instance Show Type where
  show TInt      = "Integer"
  show TBool     = "Boolean"
  show TDuration = "Duration"
  show TPitch    = "Pitch"
  show TRest     = "Rest"
  show TNote     = "Note"
  show TChord    = "Chord"
  show (TList t) = "[" ++ show t ++ "]"

  show TEmpty{} = "shouldnt show for TEmpty"
  show TError = "shouldnt show for TEmpty"

  show TData{} = "TODO show for TData"
  show TFunc{} = "TODO show for TFunc"

data Param = Param Id Type
  deriving (Eq, Ord, Show)

type Id = String

-- TODO: note that FnBody stores untyped param names (just Ids)

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
    | FnBody [Id] Expr
    | Neg Expr
    | Not Expr
    | IntOp IOpr Expr Expr
    | BoolOp BOpr Expr Expr
    | CompOp COpr Expr Expr
    | Empty
    deriving (Eq, Ord, Show)

data IOpr = Add | Sub | Mul | Div | Mod
  deriving (Eq, Ord)

instance Show IOpr where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Mod = "%"

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
showVal (VPart p)       = "Part | " ++ commaDelim p ++ " |"
showVal (VMusic m)      = "Music [" ++ commaDelim m ++ "]"
showVal (Empty)         = ""
showVal otherVal        = show otherVal

