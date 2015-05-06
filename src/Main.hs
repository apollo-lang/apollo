module Main (
  main
) where

import Control.Monad (liftM, unless)
import Control.Monad.Error (throwError)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
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
         []         -> interpret
         _          -> putStrLn "Invalid arguments"

-- Parse and evaluate a program ---------------------------------------------

interpret :: IO ()
interpret = do
  src <- getContents
  env <- nullEnv
  results <- runIOThrows $ toAst src >>= execAst env
  put results

toAst :: String -> IOThrowsError [Expr]
toAst src = liftThrows (parse src >>= \ast -> mapM_ typecheck ast >> return ast)

execAst :: Env -> [Expr] -> IOThrowsError String
execAst env ast = liftM (unlines . map showVal . filter notEmpty) (exec env ast)
    where
      notEmpty :: Expr -> Bool
      notEmpty Empty = False
      notEmpty _     = True

      exec :: Env -> [Expr] -> IOThrowsError [Expr]
      exec _   []         = return []
      exec envr (e:exprs) = do
        x <- eval envr e
        y <- exec envr exprs
        return (x:y)

put :: String -> IO ()
put r
  | '\n' `notElem` r = putStrLn r
  | otherwise        = putStr r

-- Parse a program's syntax tree --------------------------------------------

putAst :: IO ()
putAst = getContents >>= \x -> putStrLn $ case parse x of
                                 (Left  err) -> show err
                                 (Right ast) -> show ast

-- Read-Evaluate-print Loop -------------------------------------------------

runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "apollo> ") . interpretLine

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ prdc prompt action = do
  result <- prompt
  unless (prdc result)
         (action result >> until_ prdc prompt action)

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine
  where
    flushStr :: String -> IO ()
    flushStr str = putStr str >> hFlush stdout

interpretLine :: Env -> String -> IO ()
interpretLine env src = runIOThrows (toAst src >>= astCheck >>= execAst env) >>= put
    where
      astCheck :: [Expr] -> IOThrowsError [Expr]
      astCheck ast = liftThrows $
        if length ast == 1
        then return ast
        else throwError $ Default "REPL received >1 expression"

