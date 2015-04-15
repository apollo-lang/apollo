import Control.Monad (liftM, unless)
import System.Environment
import System.IO
import Parse
import Eval
import Expr
import Error

main :: IO ()
main = getArgs >>= handleArgs . getFirst
  where getFirst args = if null args then "" else head args
        handleArgs args = case args of
                            "--ast"  -> putAst
                            "--repl" -> runRepl
                            _        -> putExpr


-- Parse a program's syntax tree --------------------------------------------

putAst :: IO ()
putAst = getContents >>= print . parse


-- Parse and evaluate a program ---------------------------------------------

putExpr :: IO ()
putExpr = getContents >>= (putStr . showResults . map eval . parse)

showResults :: [ThrowsError Expr] -> String
showResults = concatMap ((++ "\n") . showResult)

showResult :: ThrowsError Expr -> String
showResult = extractValue . trapError . liftM showVal


-- Read-Evaluate-print Loop -------------------------------------------------

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "apollo> ") evalAndPrint

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ prdc prompt action = do
  result <- prompt
  unless (prdc result)
         (action result >> until_ prdc prompt action)

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

evalAndPrint :: String -> IO ()
evalAndPrint = putStrLn . evalString

evalString :: String -> String
evalString = showResult . eval . checkLength . parse
  where checkLength exprs = if length exprs == 1
                               then head exprs
                               else error "please input a single expression; got multiple"

