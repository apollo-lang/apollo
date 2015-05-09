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
    , showPP
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
    show (TFunc p t)    = "(" ++ strDelim "," show p ++ ") -> " ++ show t
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
    | Empty                     -- Value of definitions
    | Nil                       -- Value of '_' token
    deriving (Eq, Ord, Show)

-- | showPP is used for pretty-printing values after evaluation,
-- whereas the derived Show for Expr is used to print the AST

showPP :: Expr -> String
showPP (VInt  i)      = show i
showPP (VBool b)      = show b
showPP (VDuration d)  = "(" ++ show d ++ ")"
showPP (VPitch p)     = "(" ++ show p ++ ")"
showPP (VAtom p d)    = "(Atom " ++ showPP p ++ " " ++ showPP d ++ ")"
showPP (VPart p)      = "(Part " ++ strDelim " " showPP p ++ ")"
showPP (VMusic m)     = "(Music " ++ strDelim " " showPP m ++ ")"
showPP (VList l)      = "[" ++ commaDelim l  ++ "]"
showPP (Name n)       = n
showPP (Def i _ e)    = "(Def " ++ i ++ " " ++ showPP e ++ ")"
showPP (VLam is e)    = "(Lambda " ++ strDelim " " id is ++ " " ++ showPP e ++ ")"
showPP (Block es e)   = "(Block " ++ strDelim " " showPP es ++ " " ++ showPP e ++ ")"
showPP (If e1 e2 e3)  = "(If " ++ showPP e1 ++ " " ++ showPP e2 ++ " " ++ showPP e3 ++ ")"
showPP (FnCall i e)   = "(" ++ i ++ " " ++ strDelim " " showPP e ++ ")"
showPP (Neg e)        = "(Neg " ++ showPP e ++ ")"
showPP (Not e)        = "(Not " ++ showPP e ++ ")"
showPP (IntOp o a b)  = "(" ++ show o ++ " " ++ showPP a ++ " " ++ showPP b ++ ")"
showPP (BoolOp o a b) = "(" ++ show o ++ " " ++ showPP a ++ " " ++ showPP b ++ ")"
showPP (CompOp o a b) = "(" ++ show o ++ " " ++ showPP a ++ " " ++ showPP b ++ ")"
showPP _              = ""

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

strDelim :: (Show a) => String -> (a -> String) -> [a] -> String
strDelim s f = init . concatMap ((++ s) . f)

commaDelim :: [Expr] -> String
commaDelim = init . concatMap ((++ ",") . showPP)

