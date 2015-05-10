module Env (
  Env
, nullEnv
, IOThrowsError
, liftThrows
, runIOThrows
, getVar
, setVar
, defineVar
, bindVars
, isBound
, clone
) where

import Control.Monad.Error (ErrorT, throwError, runErrorT, liftM, liftIO)
import Data.Maybe (isJust)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Error

type Id = String
type Env a = IORef [(String, IORef a)]

instance Show (IORef a) where
  show _ = "<ioref>"

instance Ord (IORef a) where
  compare _ _ = EQ

nullEnv :: IO (Env a)
nullEnv = newIORef []

type IOThrowsError = ErrorT ApolloError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = liftM extractValue (runErrorT $ trapError action)

getVar :: Env a -> Id -> IOThrowsError a
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting" var)
        (liftIO . readIORef)
        (lookup var env)

setVar :: Env a -> Id -> a -> IOThrowsError a
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting" var)
        (liftIO . flip writeIORef value)
        (lookup var env)
  return value -- see TODO on defineVar

isBound :: Env a -> String -> IO Bool
isBound env var = liftM (isJust . lookup var) (readIORef env)

defineVar :: Env a -> Id -> a -> IOThrowsError a
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then throwError $ RedefVar var
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value -- TODO: could be bad; shouldnt return anything but then have to do weird stuff

bindVars :: Env a -> [(String, a)] -> IO (Env a)
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bndgs env = liftM (++ env) (mapM addBinding bndgs)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)

clone e = liftIO (readIORef e >>= newIORef)

