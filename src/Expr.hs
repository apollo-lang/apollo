
--------------------------------------------------------------------------
-- Expr: types for representing Apollo expressions
--------------------------------------------------------------------------

module Expr (
  Id
, Param(..)
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
, Atom(..)
, Music(..)
, showPP
) where

import Type
import Env

type Id = String

data Param = Param Id Type
  deriving (Eq, Ord, Show)

data Expr = VInt Int                      -- An integer
          | VBool Bool                    -- A boolean
          | VDuration Int                 -- A duration
          | VPitch Int                    -- A pitch
          | VAtom Expr Expr               -- An atom
          | VMusic [Expr]                 -- Music
          | VList [Expr]                  -- A list
          | Name Id                       -- A name
          | Def Id Type Expr              -- Definition of a name
          | VLam [Id] Expr                -- Untyped lambdas
          | VTLam [Type] Type [Id] Expr   -- Typed lambdas
          | Function [Id] Expr (Env Expr) -- A function with its closure
          | Block [Expr] Expr             -- Block
          | If Expr Expr Expr             -- Conditional
          | FnCall Expr [Expr]            -- Function call on a name or typed-lambda
          | Neg Expr                      -- Unary neg op
          | Not Expr                      -- Unary not op
          | Head Expr                     -- Unary head op
          | Tail Expr                     -- Unary tail op
          | IntOp IOpr Expr Expr          -- Infix Int op
          | BoolOp BOpr Expr Expr         -- Infix Bool op
          | CompOp COpr Expr Expr         -- Infix comparison op
          | ArrOp AOpr Expr Expr          -- Infix List op
          | Empty                         -- Value of definitions
          | Nil                           -- Value of '_' token
          deriving (Eq, Ord)

instance Show Expr where
  show (VInt  i)      = show i
  show (VBool b)      = show b
  show (VDuration d)  = "(" ++ show d ++ ")"
  show (VPitch p)     = "(" ++ show p ++ ")"
  show (VAtom p d)    = "(Atom " ++ show p ++ " " ++ show d ++ ")"
  show (VMusic m)     = "(Music " ++ strDelim " " show m ++ ")"
  show (VList es)     = "(List" ++ (strDelim " " id . map show) es  ++ ")"
  show (Name n)       = n
  show (Def i _ e)    = "(Def " ++ i ++ " " ++ show e ++ ")"
  show (VLam is e)    = "(Lambda " ++ strDelim " " id is ++ " . " ++ show e ++ ")"
  show (Block es e)   = "(Block " ++ (strDelim " " id . map show) es ++ " " ++ show e ++ ")"
  show (If e1 e2 e3)  = "(If " ++ show e1 ++ " " ++ show e2 ++ " " ++ show e3 ++ ")"
  show (FnCall e1 e2) = "(" ++ show e1 ++ " " ++ strDelim " " show e2 ++ ")"
  show (Neg e)        = "(Neg " ++ show e ++ ")"
  show (Not e)        = "(Not " ++ show e ++ ")"
  show (Head e)       = "(Head " ++ show e ++ ")"
  show (Tail e)       = "(Tail " ++ show e ++ ")"
  show (IntOp o a b)  = "(" ++ show o ++ " " ++ show a ++ " " ++ show b ++ ")"
  show (BoolOp o a b) = "(" ++ show o ++ " " ++ show a ++ " " ++ show b ++ ")"
  show (CompOp o a b) = "(" ++ show o ++ " " ++ show a ++ " " ++ show b ++ ")"
  show (ArrOp o a b)  = "(" ++ show o ++ " " ++ show a ++ " " ++ show b ++ ")"
  show Nil            = "Nil"
  show _              = "<?>"

-- showPP is used for pretty-printing values after evaluation,
-- whereas the derived Show for Expr is used to print the AST

showPP :: Expr -> String
showPP (VInt  i)      = show i
showPP (VBool b)      = show b
showPP (VDuration d)  = "(" ++ show d ++ ")"
showPP (VPitch p)     = "(" ++ show p ++ ")"
showPP (VAtom p d)    = "(Atom " ++ showPP p ++ " " ++ showPP d ++ ")"
showPP (VMusic m)     = "(Music " ++ strDelim " " showPP m ++ ")"
showPP (VList l)      = "[" ++ commaDelim l  ++ "]"
showPP (Name n)       = n
showPP (Def i _ e)    = "(Def " ++ i ++ " " ++ showPP e ++ ")"
showPP (VLam is e)    = "(Lambda " ++ strDelim " " id is ++ " . " ++ showPP e ++ ")"
showPP (Block es e)   = "(Block " ++ strDelim " " showPP es ++ " " ++ showPP e ++ ")"
showPP (If e1 e2 e3)  = "(If " ++ showPP e1 ++ " " ++ showPP e2 ++ " " ++ showPP e3 ++ ")"
showPP (FnCall e1 e2) = "(" ++ show e1 ++ " " ++ strDelim " " showPP e2 ++ ")"
showPP (Neg e)        = "(Neg " ++ showPP e ++ ")"
showPP (Not e)        = "(Not " ++ showPP e ++ ")"
showPP (Head e)       = "(Head " ++ show e ++ ")"
showPP (Tail e)       = "(Tail " ++ show e ++ ")"
showPP (IntOp o a b)  = "(" ++ show o ++ " " ++ showPP a ++ " " ++ showPP b ++ ")"
showPP (BoolOp o a b) = "(" ++ show o ++ " " ++ showPP a ++ " " ++ showPP b ++ ")"
showPP (CompOp o a b) = "(" ++ show o ++ " " ++ showPP a ++ " " ++ showPP b ++ ")"
showPP (ArrOp o a b)  = "(" ++ show o ++ " " ++ show a ++ " " ++ show b ++ ")"
showPP Nil            = "Nil"
showPP _              = "<?>"

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
data Music    = Music [[Atom]]         deriving (Eq, Ord, Show)

strDelim :: (Show a) => String -> (a -> String) -> [a] -> String
strDelim _ _ [] = ""
strDelim s f xs = init . concatMap ((++ s) . f) $ xs

commaDelim :: [Expr] -> String
commaDelim [] = ""
commaDelim xs = init . concatMap ((++ ",") . showPP) $ xs

