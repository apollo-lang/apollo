module Main
    ( main
    ) where

import Control.Monad (liftM, unless)
import Control.Monad.Trans.Error (runErrorT)
import System.Environment
import System.IO
import Parse
import Check
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
putAst = getContents >>= \x -> putStrLn $ case (parse x) of
                                 (Left  err) -> show err
                                 (Right ast) -> show ast

-- Parse and evaluate a program ---------------------------------------------

parseAndEval :: String -> IO ()
parseAndEval src = do
  env <- nullEnv
  let res = parse src >>= \ast -> mapM_ typecheck ast >> return ast
  case res of
    (Left err)    -> print err
    (Right exprs) -> execute env exprs >>= putStrLn . unlines

execute :: Env -> [Expr] -> IO [String]
execute _   []        = return []
execute env (e:exprs) =
  (runErrorT (eval env e)) >>= \res->
  case res of
    (Left err)  -> return [show err]
    (Right Empty) -> execute env exprs
    (Right val) -> liftM ([showVal val] ++) (execute env exprs)

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
evalAndPrint env str = evalString env str >>= \s -> case s of
                                                      ""       -> putStr ""
                                                      notEmpty -> putStrLn notEmpty

parseCheck :: String -> ThrowsError Expr
parseCheck s = parseRepl s >>= \ast -> typecheck ast >> return ast

evalString :: Env -> String -> IO String
evalString env expr = showResult $ (liftThrows (parseCheck expr)) >>= eval env

showResult :: IOThrowsError Expr -> IO String
showResult = runIOThrows . liftM showVal

