import System.Environment
import Parser
import Eval
import Expr

main :: IO ()
main = getArgs >>= handleArgs . getFirst
  where getFirst args = if null args then "" else head args
        handleArgs args = case args of
                            "--ast" -> parseAst
                            _       -> parseExpr

-- Evaluate a program:

parseExpr :: IO ()
parseExpr = getContents >>= putStrLn . readExpr

readExpr :: String -> String
readExpr = show . map eval . map getExpr . getStmts . parseProgram
  where getStmts (Program stmts) = stmts
        getExpr x = case x of
                      (StExp expr) -> expr
                      (StDef _)    -> error "TODO: not ready to handle Defs yet"

-- Parse a program's syntax tree:

parseAst :: IO ()
parseAst = getContents >>= putStrLn . ast

ast :: String -> String
ast = show . parseProgram

