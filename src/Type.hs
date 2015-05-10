
--------------------------------------------------------------------------
-- Type: representation of Apollo's type-system
--------------------------------------------------------------------------

module Type (
  Type(..)
) where

data Type = TInt
          | TBool
          | TDuration
          | TPitch
          | TAtom
          | TMusic
          | TList Type
          | TListEmpty
          | TNil
          | TFunc [Type] Type
          | TEmpty
          deriving (Eq, Ord)

instance Show Type where
  show TInt            = "Int"
  show TBool           = "Bool"
  show TDuration       = "Duration"
  show TPitch          = "Pitch"
  show TAtom           = "Atom"
  show TMusic          = "Music"
  show (TList t)       = "[" ++ show t ++ "]"
  show TListEmpty      = "[]"
  show (TFunc p t)     = "(" ++ strDelim ", " show p ++ ") -> " ++ show t
  show _               = "<!>"

strDelim :: (Show a) => String -> (a -> String) -> [a] -> String
strDelim _ _ []  = ""
strDelim s f xs = init . init . concatMap ((++ s) . f) $ xs

