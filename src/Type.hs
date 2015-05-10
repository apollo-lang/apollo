module Type (
 Type(..)
) where

data Type
    = TInt
    | TBool
    | TDuration
    | TPitch
    | TAtom
    | TMusic
    | TList Type
    | TListEmpty
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
    show TMusic         = "Music"
    show (TList t)      = "[" ++ show t ++ "]"
    show TListEmpty = "[]"
    show TEmpty{}  = "shouldnt show for TEmpty" -- TODO: remove
    show TError    = "shouldnt show for TEmpty" -- TODO: remove
    show (TFunc p t)    = "(" ++ strDelim "," show p ++ ") -> " ++ show t
    show _              = ""

strDelim :: (Show a) => String -> (a -> String) -> [a] -> String
strDelim s f = init . concatMap ((++ s) . f)
