module Main (
  main
) where

import Control.Monad (liftM, unless, when)
import Control.Monad.Error (throwError, liftIO, runErrorT)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Data.IORef (readIORef)
import Parse
import Check
import Error
import Type
import Expr
import Eval
import Env
import Util
import Midi
import Lib

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
  results <- runIOThrows $ toAst typeEnv (prelude ++ src) >>= execAst env >>= \r -> handleMain env "main.mid" >> return r
  put results

handleMain :: Env Expr -> String -> IOThrowsError ()
handleMain env filename = do
  mainExists <- liftIO (isBound env "main")
  tempo <- getTempo env
  liftIO $ when mainExists (export tempo)
    where
      export tempo = getMain >>= \m -> exportMusic tempo filename (makeMusic m)
      getMain = runUnsafe (getVar env "main")
      runUnsafe action = liftM extractValue (runErrorT action)

getTempo :: Env Expr -> IOThrowsError Int
getTempo env = do
  tempoExists <- liftIO (isBound env "#tempo")
  VInt tempo <- if tempoExists
                then getVar env "#tempo"
                else return defaultTempo
  return tempo
    where defaultTempo = VInt 120

toAst :: Env Type -> String -> IOThrowsError [Expr]
toAst env src = liftThrows (parse src) >>= \ast -> mapM_ (typecheck env) ast >> return ast

execAst :: Env Expr -> [Expr] -> IOThrowsError String
execAst env ast = liftM (unlines . map showPP . filter notEmpty) (exec env ast)
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

  _ <- runIOThrows $ toAst tEnv prelude >>= execAst env

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
interpretLine env tEnv inp =
  if inp == ":browse" -- TODO: strip whitespace
  then getBindings >>= putStr . unlines
  else runIOThrows (toAst tEnv inp >>= astCheck >>= execAst env) >>= put
    where
      astCheck :: [Expr] -> IOThrowsError [Expr]
      astCheck ast = liftThrows $
        if length ast == 1
        then return ast
        else throwError $ Default "REPL received >1 expression"
      getBindings = do
        e <- readIORef tEnv
        mapM listing e
      listing (name, typ) = readIORef typ >>= \t -> return (name ++ " : " ++ show t)
      -- IO [(s, t)]

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

