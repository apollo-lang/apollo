
--------------------------------------------------------------------------
-- Main: interpreter, repl, and command-line interfaces
--------------------------------------------------------------------------

module Main (
  main
) where

import Control.Monad (liftM, unless, when)
import Control.Monad.Error (throwError, liftIO, runErrorT)
import System.Environment (getArgs)
import System.IO (hFlush, stdout, isEOF)
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
         ["--help"] -> usage
         ["-h"]     -> usage
         ["-"]      -> getContents >>= interpret "main.mid"
         [fname]    -> readFile fname >>= interpret "main.mid"
         [fname, "-o", ofile] -> readFile fname >>= interpret ofile
         _          -> putStrLn "Invalid arguments"

-- Parse and evaluate a program ---------------------------------------------

interpret :: String -> String -> IO ()
interpret ofile src = do
  env <- nullEnv
  typeEnv <- nullEnv
  _ <- loadPrelude typeEnv env
  results <- runIOThrows (interp typeEnv env)
  put results
    where
      interp t e = toAst t src >>=
                   execAst e >>= \r ->
                   handleExport e ofile "main" >>
                   return r

loadPrelude :: Env Type -> Env Expr -> IO String
loadPrelude typeEnv env = runIOThrows $ toAst typeEnv prelude >>= execAst env

handleExport :: Env Expr -> String -> String -> IOThrowsError ()
handleExport env filename mainName = do
  mainExists <- liftIO (isBound env mainName)
  tempo <- getTempo env
  liftIO $ when mainExists (export tempo >> putStrLn ("Music in `" ++ mainName ++ "` exported to " ++ filename))
    where
      export tempo = getMain >>= \m -> exportMusic tempo filename (makeMusic m)
      getMain = runUnchecked (getVar env mainName)
      runUnchecked action = liftM extractValue (runErrorT action)

getTempo :: Env Expr -> IOThrowsError Int
getTempo env = do
  tempoExists <- liftIO (isBound env "#tempo")
  VInt tempo <- if tempoExists
                then getVar env "#tempo"
                else return defaultTempo
  return tempo
    where
      defaultTempo = VInt 120

toAst :: Env Type -> String -> IOThrowsError [Expr]
toAst env src = liftThrows (parse src) >>= \ast -> mapM_ (typecheck env) ast >> return ast

execAst :: Env Expr -> [Expr] -> IOThrowsError String
execAst env ast = liftM (unlines . map showPP . filter notEmpty) (exec env ast)
  where
    notEmpty Empty = False
    notEmpty _     = True
    exec _   []         = return []
    exec envr (e:exprs) = do
      x <- eval envr e
      y <- exec envr exprs
      return (x:y)

put :: String -> IO ()
put r | r == ""          = putStr r
      | '\n' `notElem` r = putStrLn r
      | otherwise        = putStr r

-- Parse a program's syntax tree --------------------------------------------

putAst :: IO ()
putAst = getContents >>= \x -> putStrLn $ case parse x of
                                 Left  err -> show err
                                 Right ast -> show ast

-- Read-Evaluate-print Loop -------------------------------------------------

runRepl :: IO ()
runRepl = do
  env  <- nullEnv
  tEnv <- nullEnv
  _ <- loadPrelude tEnv env
  _ <- replUsage
  until_ (== ":quit") (readPrompt "apollo> ") (interpretLine env tEnv)

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ prdc prompt action = do
  result <- prompt
  unless (prdc result)
         (action result >> until_ prdc prompt action)

readPrompt :: String -> IO String
readPrompt prompt = do
  _ <- flushStr prompt
  end <- isEOF
  if end
  then return ":quit"
  else getLine
  where
    flushStr str = putStr str >> hFlush stdout

interpretLine :: Env Expr -> Env Type -> String -> IO ()
interpretLine env tEnv inp = case inp of
  ":browse" -> getBindings >>= putStr . unlines
  (':':'e':'x':'p':'o':'r':'t':' ':name) -> runUnchecked (handleExport env "_repl.mid" name)
  src -> runIOThrows (toAst tEnv src >>= astCheck >>= execAst env) >>= put
 where
  astCheck ast = liftThrows $ if length ast == 1
                              then return ast
                              else
                                if null ast
                                then throwError $ Default ""
                                else throwError $ Default "REPL received >1 expression"
  getBindings = do
    e <- readIORef tEnv
    mapM listing e
  listing (name, typ) = readIORef typ >>= \t -> return (name ++ " : " ++ show t)
  runUnchecked action = liftM extractValue (runErrorT action)

-- Help interface -----------------------------------------------------------

usage :: IO()
usage = do
  putStrLn "Apollo: a language for algorithmic music composition"
  putStrLn ""
  putStrLn "Usage: apollo [options|-] <source file> [-o <output>]"
  putStrLn ""
  putStrLn "Options:"
  putStrLn "       --repl      Start Read-Evaluate-Print-Loop"
  putStrLn "       --ast       Print the abstract syntax tree of source input through stdin"
  putStrLn "    -h|--help      Print this message"
  putStrLn "    -o <output>    Output midi to specified filename if source file present"
  putStrLn "       -           Read from stdin"

replUsage :: IO ()
replUsage = do
  putStrLn $ "Apollo repl, version " ++ showVersion ++ ": https://github.com/apollo-lang/apollo"
  putStrLn   ""
  putStrLn   "Commands:"
  putStrLn   "  :browse            See all current bindings and their types"
  putStrLn   "  :export <name>     Export a name of type Music to `_repl.mid`"
  putStrLn   "  :quit              Exit the repl"
  putStrLn   ""
    where
      showVersion = init (concatMap ((++ ".") . show) version)
      version = [0,0,1,0] :: [Int]

