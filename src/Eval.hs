module Eval
    ( eval
    ) where

import Control.Monad (liftM, liftM2)
import Control.Monad.Error (throwError, liftIO)
import Data.IORef (newIORef, readIORef)
import Error
import Expr
import Env

eval :: Env Expr -> Expr -> IOThrowsError Expr
eval env expr = case expr of

  VInt i      -> return $ VInt i
  VBool b     -> return $ VBool b
  VPitch p    -> return $ VPitch p
  VDuration d -> return $ VDuration d

  VAtom a b -> liftM2 VAtom (eval env a) (eval env b)

  VPart p -> liftM VPart (mapM (evalP env) p)

  VMusic m -> liftM VMusic (mapM (evalM env) m)

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
    return . VInt $ negate i

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
    if restricted op && isZero b'
    then throwError DivByZero
    else matchI op a' b'
      where
        restricted Div = True
        restricted Mod = True
        restricted _   = False
        isZero (VInt x)                 = x == 0
        isZero (VPitch (Pitch x))       = x == 0
        isZero (VDuration (Duration x)) = x == 0

  ArrOp op a l -> do
    a' <- eval env a
    (VList l') <- eval env l
    return . VList $ a' : l'

  VList xs -> liftM VList (mapM (eval env) xs)

  Block body ret -> do
    closure <- liftIO (foldr addBinding (readIORef env) body >>= newIORef)
    eval closure ret
      where
        addBinding (Def name _ ex) envMap = liftM2 (:) (makeRef name ex) envMap
        makeRef name ex = newIORef ex >>= \e -> return (name, e)

  FnBody _ body -> eval env body

  Def name _ ex -> defineVar env name ex >> return Empty

  Name name -> getVar env name >>= eval env

  FnCall name args -> do
    FnBody params body <- getVar env name
    args' <- mapM (eval env) args
    apply params body env args'

  Empty -> error "Error: eval called on Empty"

  Nil   -> return Nil


evalP :: Env Expr -> Expr -> IOThrowsError Expr
evalP env expr = case expr of
  VAtom a b  -> return $ VAtom a b
  Name name -> getVar env name >>= evalP env
  _        -> throwError $ Default "Error: expected Note, Rest or Chord"

evalM :: Env Expr -> Expr -> IOThrowsError Expr
evalM env expr = case expr of
  VPart p -> liftM VPart (mapM (evalP env) p)
  Name name -> getVar env name >>= evalM env
  _        -> throwError $ Default "Error: expected Part"

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

matchI _ _ _ = error "TODO this should be taken care of in typechecking"
  -- throwError $ TypeMismatch (show op) (typeOf a) (show b)

applyI :: IOpr -> Int -> Int -> Int
applyI op a b = case op of
  Add -> a + b
  Mul -> a * b
  Sub -> a - b
  Div -> a `div` b
  Mod -> a `mod` b

apply :: [Id] -> Expr -> Env Expr -> [Expr] -> IOThrowsError Expr
apply params body closure args =
  createEnv args >>= flip eval body
    where
      createEnv = liftIO . bindVars closure . zip params

