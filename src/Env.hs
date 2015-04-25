module Env
    ( Env
    , nullEnv
    , IOThrowsError
    , liftThrows
    , runIOThrows
    , getVar
    , defineVar
    , bindVars
    ) where

import Control.Monad.Error (ErrorT, throwError, runErrorT, liftM, liftIO)
import Data.Maybe (isJust)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Error
import Expr

type Env = IORef [(String, IORef (Type, Expr))]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ErrorT ApolloError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = liftM extractValue (runErrorT $ trapError action)

isBound :: Env -> String -> IO Bool
isBound envRef var = liftM (isJust . lookup var) (readIORef envRef)

getVar :: Env -> String -> IOThrowsError (Type, Expr)
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting" var)
        (liftIO . readIORef)
        (lookup var env)

setVar :: Env -> String -> (Type, Expr) -> IOThrowsError Expr
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting" var)
        (liftIO . flip writeIORef value)
        (lookup var env)
  return Empty

defineVar :: Env -> String -> (Type, Expr) -> IOThrowsError Expr
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then throwError $ RedefVar var
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return Empty

bindVars :: Env -> [(String, (Type, Expr))] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bndgs env = liftM (++ env) (mapM addBinding bndgs)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)

