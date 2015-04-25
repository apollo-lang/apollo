module Eval
    ( eval
    ) where

import Control.Monad (liftM)
import Control.Monad.Error (throwError, liftIO)
import Error
import Expr
import Env

eval :: Env -> Expr -> IOThrowsError Expr
eval env expr = case expr of
  VInt i -> return $ VInt i

  VBool b -> return $ VBool b

  VPitch p -> return $ VPitch p

  VDuration d -> return $ VDuration d

  VRest r -> return $ VRest r

  VNote n -> return $ VNote n

  VChord c -> return $ VChord c

  VList xs -> liftM VList (mapM (eval env) xs)

  If test tr fl -> do
    VBool b <- eval env test
    if b
    then eval env tr
    else eval env fl

  Not e -> do
    VBool b <- eval env e
    return . VBool $ not b

  Neg e -> do
    VInt i <- eval env e
    return . VInt $ -i

  BoolOp op a b -> do
    VBool a' <- eval env a
    VBool b' <- eval env b
    return . VBool $ applyB op a' b'

  CompOp op a b -> do
    a' <- eval env a
    b' <- eval env b
    return . VBool $ applyC op a' b'

  IntOp op a b -> do
    a' <- eval env a
    b' <- eval env b
    matchI op a' b'

  Block body ret -> mapM_ (eval env) body >> eval env ret

  Def name typ ex -> defineVar env name (typ, ex)

  Name name -> getVar env name >>= eval env . snd

  FnCall name args -> do
    ((TFunc params _), body) <- getVar env name
    args' <- mapM (eval env) args
    apply name params body env args'

  Empty    -> throwError $ Default "Error: eval called on Empty"

applyB :: BOpr -> Bool -> Bool -> Bool
applyB op a b = case op of
  And -> a && b
  Or  -> a || b

applyC :: COpr -> Expr -> Expr -> Bool
applyC op a b = case op of
  Eq  -> a == b
  NEq -> a /= b
  Le  -> a < b
  Gr  -> a > b
  LEq -> a <= b
  GEq -> a >= b

matchI :: IOpr -> Expr -> Expr -> IOThrowsError Expr
matchI op (VInt a) (VInt b) =
  return . VInt $ applyI op a b

matchI op (VPitch (Pitch a)) (VPitch (Pitch b)) =
  return . VPitch . Pitch . (`mod` pitchLimit) $ applyI op a b
    where pitchLimit = 128

matchI op (VDuration (Duration a)) (VDuration (Duration b)) =
  return . VDuration . Duration $ applyI op a b

matchI op a b =
  throwError $ TypeMismatch (show op) (typeOf a) (b)

applyI :: IOpr -> Int -> Int -> Int
applyI op a b = case op of
  Add -> a + b
  Mul -> a * b
  Sub -> a - b
  Div -> a `div` b
  Mod -> a `mod` b

apply :: String -> [Param] -> Expr -> Env -> [Expr] -> IOThrowsError Expr
apply name paramList body closure args =
  if num params /= num args
  then throwError $ ArgMismatch name (num params) args
  else createEnv args >>= (flip eval) body
    where num = toInteger . length
          params = map (\(Param n _) -> n) paramList
          createEnv = liftIO . bindVars closure . zip params . map (\a -> (TData "TODO", a))

