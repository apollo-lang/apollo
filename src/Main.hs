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
import Util
import Midi

main :: IO ()
main = getArgs >>= \args ->
       case args of
         ["--ast"]  -> putAst
         ["--repl"] -> runRepl
         ["-h"]     -> runHelp
         ["--help"] -> runHelp
         []         -> interpret
         _          -> putStrLn "Invalid arguments"

-- Parse and evaluate a program ---------------------------------------------

interpret :: IO ()
interpret = do
  src <- getContents
  env <- nullEnv
  typeEnv <- nullEnv
  results <- runIOThrows $ toAst typeEnv src >>= execAst env
  checkMain <- isBound env "main"
  checkTemp <- isBound env "#tempo"
  if checkMain then do
    m <- (runTypeExpr $ getVar env "main")  
    if checkTemp then do
      t <- (runTypeExpr $ getVar env "#tempo")
      (VInt v) <- (runTypeExpr $ eval env t)
      (runTypeExpr $ eval env m) >>= exportMusic v "main.mid" . makeMusic
    else
      (runTypeExpr $ eval env m) >>= exportMusic 120 "main.mid" . makeMusic
  else return ()
  put results

toAst :: Env Type -> String -> IOThrowsError [Expr]
toAst env src = liftThrows (parse src) >>= \ast -> mapM_ (typecheck env) ast >> return ast

execAst :: Env Expr -> [Expr] -> IOThrowsError String
execAst env ast = liftM (unlines . map show . filter notEmpty) (exec env ast)
    where
      notEmpty :: Expr -> Bool
      notEmpty Empty = False
      notEmpty _     = True

      exec :: Env Expr -> [Expr] -> IOThrowsError [Expr]
      exec _   []         = return []
      exec envr (e:exprs) = do
        x <- eval envr e
        y <- exec envr exprs
        return (x:y)

put :: String -> IO ()
put r
  | r == ""          = putStr r
  | '\n' `notElem` r = putStrLn r
  | otherwise        = putStr r

-- Parse a program's syntax tree --------------------------------------------

putAst :: IO ()
putAst = getContents >>= \x -> putStrLn $ case parse x of
                                 (Left  err) -> show err
                                 (Right ast) -> show ast

-- Read-Evaluate-print Loop -------------------------------------------------

runRepl :: IO ()
runRepl = do
  env  <- nullEnv
  tEnv <- nullEnv
  until_ (== "quit") (readPrompt "apollo> ") (interpretLine env tEnv)

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

interpretLine :: Env Expr -> Env Type -> String -> IO ()
interpretLine env tEnv src =
  runIOThrows (toAst tEnv src >>= astCheck >>= execAst env) >>= put
    where
      astCheck :: [Expr] -> IOThrowsError [Expr]
      astCheck ast = liftThrows $
        if length ast == 1
        then return ast
        else throwError $ Default "REPL received >1 expression"

-- Help interface -----------------------------------------------------------

runHelp :: IO()
runHelp = do
  putStrLn "Apollo: algorithmic music composition"
  putStrLn ""
  putStrLn "Usage: apollo [options] < sourcefile.ap"
  putStrLn "    --repl    Start Apollo interactive mode (Read-Evaluate-Print-Loop)"
  putStrLn "    --ast     Print a program's abstract syntax tree"
  putStrLn "    --help    This help message"
  putStrLn ""

