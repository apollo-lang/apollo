import Control.Monad (liftM)
import System.Environment
import Parser
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
parseAst = show . parseProgram

-- Parse and evaluate a program:

putExpr :: IO ()
putExpr = do
  src <- getContents
  results <- return $ parseExpr src
  putStr . printList $ map showResult results

parseExpr :: String -> [ThrowsError Expr]
parseExpr = map (eval . getExpr) . getStmts . parseProgram
  where getStmts (Program stmts) = stmts
        getExpr x = case x of
                      (StExp expr) -> expr
                      (StDef _)    -> error "TODO: `Def` not implemented"

showResult :: ThrowsError Expr -> String
showResult = extractValue . trapError . (liftM show)

printList :: [String] -> String
printList = foldr1 (++) . map (++ "\n")

