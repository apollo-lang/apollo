module Main
( main
) where
import Control.Monad (liftM, unless)
import Control.Monad.Trans.Error (runErrorT)
import System.Environment
import System.IO
import Parse
import Expr
import Eval
import Env

main :: IO ()
main = getArgs >>= \args ->
       case args of
         ["--ast"]  -> putAst
         ["--repl"] -> runRepl
         []         -> getContents >>= parseAndEval
         _          -> putStrLn "Invalid arguments"

-- Parse a program's syntax tree --------------------------------------------

putAst :: IO ()
putAst = getContents >>= print . parse

-- Parse and evaluate a program ---------------------------------------------

parseAndEval :: String -> IO ()
parseAndEval src = do
  env <- nullEnv
  case (parse src) of
    (Left err)    -> putStrLn $ show err
    (Right exprs) -> execute env exprs >>= putStrLn . concatMap (++ "\n")

-- TODO: does this allow env to be mutated? (never explicitly passed to recurse
-- in new form). can we maybe use a simpler state representation and just pass
-- it out of eval (along with play's durrent value)

execute :: Env -> [Expr] -> IO [String]
execute _   []        = return []
execute env (e:exprs) =
  (runErrorT (eval env e)) >>= \res->
  case res of
    (Left err)  -> return $ [show err]
    (Right val) -> liftM ([showVal val] ++) (execute env exprs)

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

