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
    (Left err)    -> undefined
    (Right exprs) -> nullEnv >>= \env -> execute "" env exprs >>= putStrLn

-- TODO: does this allow env to be mutated? (never explicitly passed to recurse
-- in new form). can we maybe use a simpler state representation and just pass
-- it out of eval (along with play's durrent value)

execute :: String -> Env -> [Expr] -> IO String
-- execute _   [] = undefined
execute acc env (e:exprs) =
  case (eval env e) of
    (Left err)  -> showResult err >>= \e -> return $ addResult e acc
    (Right val) -> showResult val >>= \v -> execute (addResult v acc) env exprs

addResult :: String -> String -> String
addResult str accum = str ++ "\n" ++ accum

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

