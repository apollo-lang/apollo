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
    | TNil
    | TErrVerbose String   -- TODO: remove
    | TError          -- TODO: remove
    | TFunc [Type] Type
    | TEmpty
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
    show (TErrVerbose t)  = "VERBOSE ERROR <" ++ show t ++ ">" -- TODO: remove
    show TError    = "shouldnt show for TError" -- TODO: remove
    show (TFunc p t)    = "(" ++ strDelim ", " show p ++ ") -> " ++ show t
    show _              = "<!>"

strDelim :: (Show a) => String -> (a -> String) -> [a] -> String
strDelim s f = init . init . concatMap ((++ s) . f)

