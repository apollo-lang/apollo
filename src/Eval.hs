module Eval
    ( eval
    ) where

import Control.Monad (liftM, liftM2)
import Control.Monad.Error (throwError, liftIO)
import Error
import Expr
import Env

eval :: Env Expr -> Expr -> IOThrowsError Expr
eval env expr = case expr of

  VInt i      -> return $ VInt i
  VBool b     -> return $ VBool b
  VPitch p    -> return $ VPitch p
  VDuration d -> return $ VDuration d

  VRest r -> return $ VRest r

  VNote n -> return $ VNote n

  VChord c -> return $ VChord c

  VAtom a b -> liftM2 VAtom (eval env a) (eval env b)

  VPart p -> liftM VPart (mapM (evalP env) p)

  VMusic m -> liftM VMusic (mapM (evalM env) m)

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

  VList xs -> liftM VList (mapM (eval env) xs)

  Block body ret -> mapM_ (eval env) body >> eval env ret

  FnBody _ body -> eval env body

  Def name _ ex -> defineVar env name ex >> return Empty

  Name name -> getVar env name >>= eval env

  FnCall name args -> do
    FnBody params body <- getVar env name
    args' <- mapM (eval env) args
    apply name params body env args'

  Empty -> error "Error: eval called on Empty"

  Nil   -> return Nil

evalP :: Env Expr -> Expr -> IOThrowsError Expr
evalP _ expr = case expr of
  VRest r  -> return $ VRest r
  VNote n  -> return $ VNote n
  VChord c -> return $ VChord c
  _        -> throwError $ Default "Error: expected Note, Rest or Chord"

evalM :: Env Expr -> Expr -> IOThrowsError Expr
evalM env expr = case expr of
  VPart p -> liftM VPart (mapM (evalP env) p)
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

matchI op a b = error "TODO this should be taken care of in typechecking"
  -- throwError $ TypeMismatch (show op) (typeOf a) (show b)

applyI :: IOpr -> Int -> Int -> Int
applyI op a b = case op of
  Add -> a + b
  Mul -> a * b
  Sub -> a - b
  Div -> a `div` b
  Mod -> a `mod` b

apply :: Id -> [Id] -> Expr -> Env Expr -> [Expr] -> IOThrowsError Expr
apply name params body closure args =
  createEnv args >>= (flip eval) body
    where
      createEnv = liftIO . bindVars closure . zip params

