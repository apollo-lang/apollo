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
    | TFunc [Type] Type
    deriving (Eq, Ord)

instance Show Type where
    show TInt           = "Int"
    show TBool          = "Bool"
    show TDuration      = "Duration"
    show TPitch         = "Pitch"
    show TAtom          = "Atom"
    show TPart          = "Part"
    show TMusic         = "Music"
    show (TList t)      = "[" ++ show t ++ "]"
    show (TFunc p t)    = "(" ++ strdelim "," show p ++ ") -> " ++ show t
    show _              = ""

data Param = Param Id Type
    deriving (Eq, Ord, Show)

type Id = String

data Expr
    = VInt Int
    | VBool Bool
    | VDuration Duration
    | VPitch Pitch
    | VAtom Expr Expr
    | VPart [Expr]
    | VMusic [Expr]
    | VList [Expr]
    | Name Id
    | Def Id Type Expr
    | VLam [Id] Expr            -- Lambdas / Functions
    | Block [Expr] Expr
    | If Expr Expr Expr
    | FnCall Id [Expr]
    | Neg Expr
    | Not Expr
    | IntOp IOpr Expr Expr
    | BoolOp BOpr Expr Expr
    | CompOp COpr Expr Expr
    | ArrOp AOpr Expr Expr
    | Empty                     -- Value of definitions
    | Nil                       -- Value of '_' token
    deriving (Eq, Ord)

instance Show Expr where
    show (VInt  i)      = show i
    show (VBool b)      = show b
    show (VDuration d)  = "(" ++ show d ++ ")"
    show (VPitch p)     = "(" ++ show p ++ ")"
    show (VAtom p d)    = "(Atom " ++ show p ++ " " ++ show d ++ ")"
    show (VPart p)      = "(Part " ++ strdelim " " show p ++ ")"
    show (VMusic m)     = "(Music " ++ strdelim " " show m ++ ")"
    show (VList l)      = "(List " ++ strdelim " " show l  ++ ")"
    show (Name n)       = n
    show (Def i _ e)    = "(Def " ++ i ++ " " ++ show e ++ ")"
    show (VLam is e)    = "(Lambda " ++ strdelim " " id is ++ " " ++ show e ++ ")"
    show (Block es e)   = "(Block " ++ strdelim " " show es ++ " " ++ show e ++ ")"
    show (If e1 e2 e3)  = "(If " ++ show e1 ++ " " ++ show e2 ++ " " ++ show e3 ++ ")"
    show (FnCall i e)   = "(" ++ i ++ " " ++ strdelim " " show e ++ ")"
    show (Neg e)        = "(Neg " ++ show e ++ ")"
    show (Not e)        = "(Not " ++ show e ++ ")"
    show (IntOp o a b)  = "(" ++ show o ++ " " ++ show a ++ " " ++ show b ++ ")"
    show (BoolOp o a b) = "(" ++ show o ++ " " ++ show a ++ " " ++ show b ++ ")"
    show (CompOp o a b) = "(" ++ show o ++ " " ++ show a ++ " " ++ show b ++ ")"
    show _              = ""

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

strdelim :: (Show a) => String -> (a -> String) -> [a] -> String
strdelim = \s -> \f -> init . concatMap ((++ s) . f)

