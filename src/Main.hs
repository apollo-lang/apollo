import Parser
import Eval
import Expr

main :: IO ()
main = parseExpr

-- Assign `main` to `parseExpr` to evaluate a program:

parseExpr :: IO ()
parseExpr = getContents >>= putStrLn . readExpr

readExpr :: String -> String
readExpr = show . map eval . map getExpr . getStmts . parseProgram
  where getStmts (Program stmts) = stmts
        getExpr x = case x of
                      (StExp expr) -> expr
                      (StDef _)    -> error "TODO: not ready to handle Defs yet"

-- Assign `main` to `parseAst` to parse a program's syntax tree:

parseAst :: IO ()
parseAst = getContents >>= putStrLn . ast

ast :: String -> String
ast = show . parseProgram

