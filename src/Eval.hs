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

  VMusic m -> liftM VMusic (mapM (evalM env) m)

  If test tr fl -> do
    VBool b <- eval env test
    if b
    then eval env tr
    else eval env fl

  Not e -> do
    e' <- eval env e
    case e' of 
      VBool b -> return . VBool $ not b
      VList l -> return . VBool $ null l
      _       -> error "Error: expected Bool, Part or List" 

  Neg e -> do
    VInt i <- eval env e
    return . VInt $ negate i

  Head l -> do
    l' <- eval env l
    case l' of 
      VList ll -> return (head ll)
      _       -> error "Error: expected Part or List" 

  Tail l -> do
    l' <- eval env l
    case l' of 
      VList (x:xs) -> return (VList xs)
      _       -> error "Error: expected Part or List" 

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
        isZero (VInt x)      = x == 0
        isZero (VPitch x)    = x == 0
        isZero (VDuration x) = x == 0

  ArrOp op a l -> do
    a' <- eval env a
    l' <- eval env l
    case l' of 
      VList ll -> return . VList $ a' : ll
      _        -> error "Error: expected Part or List" 

  VList xs -> liftM VList (mapM (eval env) xs)

  Block body ret -> do
    env' <- clone env
    mapM_ (eval env') body
    eval env' ret
      where
        clone e = liftIO (readIORef e >>= newIORef . removeNames)
        removeNames = filter (\(n,_) -> n `notElem` names)
        names = map (\(Def name _ _) -> name) body

  VLam params body -> clone env >>= \closure -> return (Function params body closure)

  Function{} -> error "bug / TODO(?): eval called on Function"

  -- HAHA RECURSION YOU SUCKA
  -- For recursion, binding names must be initialized
  -- before they are stored.

  Def name _ ex@(VLam p b) -> do
    val <- eval env ex
    defineVar env name Empty
    env' <- clone env
    setVar env' name (Function p b env')
    return Empty

  Def name _ ex -> eval env ex >>= defineVar env name >> return Empty

  Name name -> getVar env name

  FnCall name args -> do
    Function p b closure <- getVar env name
    args' <- mapM (eval env) args
    apply p b closure args'

  Empty -> error "Error: eval called on Empty"

  Nil   -> return Nil


evalP :: Env Expr -> Expr -> IOThrowsError Expr
evalP env expr = case expr of
  VAtom a b  -> eval env $ VAtom a b
  Name name -> getVar env name >>= evalP env
  _        -> throwError $ Default "Error: expected Note, Rest or Chord"

evalM :: Env Expr -> Expr -> IOThrowsError Expr
evalM env expr = case expr of
  VList p -> liftM VList (mapM (evalP env) p)
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

matchI op (VPitch a) (VPitch b) =
  return . VPitch . (`mod` 128) $ applyI op a b

matchI op (VInt a) (VPitch b) =
  return . VPitch . (`mod` 128) $ applyI op a b

matchI op (VPitch a) (VInt b) =
  return . VPitch . (`mod` 128) $ applyI op a b

matchI op (VDuration a) (VDuration b) =
  return . VDuration $ applyI op a b

matchI op (VDuration a) (VInt b) =
  return . VDuration $ applyI op a b

matchI op (VInt a) (VDuration b) =
  return . VDuration $ applyI op a b

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

