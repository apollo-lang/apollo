import Control.Monad (liftM)
import System.Environment
import Parse
import Eval
import Expr
import Error

main :: IO ()
main = getArgs >>= handleArgs . getFirst
  where getFirst args = if null args then "" else head args
        handleArgs args = case args of
                            "--ast" -> putAst
                            _       -> putExpr

-- Parse a program's syntax tree:

putAst :: IO ()
putAst = getContents >>= putStrLn . parseAst

parseAst :: String -> String
parseAst = show . parse

-- Parse and evaluate a program:

putExpr :: IO ()
putExpr = getContents >>= (putStr . showResults . map eval . parse)

showResults :: [ThrowsError Expr] -> String
showResults = concat . map ((++ "\n") . showResult)

showResult :: ThrowsError Expr -> String
showResult = extractValue . trapError . liftM show

