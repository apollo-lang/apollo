{-# OPTIONS_GHC -fno-warn-orphans #-}

-------------------------------------------------------------------------------
-- Env: module for representing an environment in a mutable symbol-table
-------------------------------------------------------------------------------

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
import Control.Monad.IO.Class (MonadIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
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
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = liftM extractValue (runErrorT (trapError action))

getVar :: Env a -> Id -> IOThrowsError a
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting" var)
        (liftIO . readIORef)
        (lookup var env)

setVar :: Env a -> Id -> a -> IOThrowsError ()
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting" var)
        (liftIO . flip writeIORef value)
        (lookup var env)
  return ()

isBound :: Env a -> String -> IO Bool
isBound env var = liftM (isJust . lookup var) (readIORef env)

defineVar :: Env a -> Id -> a -> IOThrowsError ()
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then throwError $ RedefVar var
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return ()

bindVars :: Env a -> [(String, a)] -> IOThrowsError (Env a)
bindVars envRef bindings =
  liftIO (readIORef envRef >>= extendEnv bindings >>= newIORef)
    where
      extendEnv bndgs env = liftM (++ env) (mapM addBinding bndgs)
      addBinding (var, value) = liftM ((,) var) (newIORef value)

clone :: MonadIO m => IORef a -> m (IORef a)
clone e = liftIO (readIORef e >>= newIORef)

