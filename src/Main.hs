module Main
( main
) where
import Control.Monad (liftM, unless)
import System.Environment
import System.IO
import Parse
import Error
import Expr
import Eval
import Env

main :: IO ()
main = getArgs >>= \args ->
       case args of
         ["--ast"]  -> putAst
         ["--repl"] -> runRepl
--         []         -> putExpr
         _          -> putStrLn "Invalid arguments"

-- Parse a program's syntax tree --------------------------------------------

putAst :: IO ()
putAst = getContents >>= print . parse

-- Parse and evaluate a program ---------------------------------------------

{-
putExpr :: IO ()
putExpr = do
  src <- getContents
  env <- nullEnv
  let exprs = liftThrows $ parse src
  let results = map (eval env) exprs
  putStr $ showResults results

showResults :: [IOThrowsError Expr] -> IO String
showResults = mapM showResult >>= return . concatMap (++ "\n")
-}

showResult :: IOThrowsError Expr -> IO String
showResult = runIOThrows . liftM showVal

-- Read-Evaluate-print Loop -------------------------------------------------

runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "apollo> ") . evalAndPrint

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ prdc prompt action = do
  result <- prompt
  unless (prdc result)
         (action result >> until_ prdc prompt action)

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env str = evalString env str >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = showResult $ (liftThrows $ parseRepl expr) >>= eval env

