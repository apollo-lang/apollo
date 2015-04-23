module Eval
( eval
) where
import Control.Monad (liftM)
import Expr
import Env
import Types

eval :: Env -> Expr -> IOThrowsError Expr
eval env expr = case expr of
  VInt i -> return $ VInt i

  VBool b -> return $ VBool b

  VPitch p -> return $ VPitch p

  VDuration d -> return $ VDuration d

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

  IntOp op a b -> do
    VInt a' <- eval env a
    VInt b' <- eval env b
    return . VInt $ applyI op a' b'

  BoolOp op a b -> do
    VBool a' <- eval env a
    VBool b' <- eval env b
    return . VBool $ applyB op a' b'

  PitchOp op a b -> do
    VPitch a' <- eval env a
    VPitch b' <- eval env b
    return . VPitch $ applyP op a' b'

  Block body ret -> mapM (eval env) body >> eval env ret

  Def name typ e -> do
    ex <- eval env e
    defineVar env name ex

  Name name -> getVar env name

  -- TODO: handle cases with undefined; also handle errors
  other -> error $ "not yet implemented: " ++ show other

applyI :: IOpr -> Int -> Int -> Int
applyI op a b = case op of
  Add -> a + b
  Mul -> a * b
  Sub -> a - b
  Div -> a `div` b
  Mod -> a `mod` b

applyB :: BOpr -> Bool -> Bool -> Bool
applyB op a b = case op of
  Eq  -> a == b
  NEq -> a /= b
  Le  -> a < b
  Gr  -> a > b
  LEq -> a <= b
  GEq -> a >= b
  And -> a && b
  Or  -> a || b

applyP :: IOpr -> Pitch -> Pitch -> Pitch
applyP op a b = case op of
  Add -> Pitch $ ((pitch a) + (pitch b)) `mod` 128
  Mul -> Pitch $ ((pitch a) * (pitch b)) `mod` 128
  Sub -> Pitch $ ((pitch a) - (pitch b)) `mod` 128
  Div -> Pitch $ (pitch a) `div` (pitch b)
  Mod -> Pitch $ (pitch a) `mod` (pitch b)

applyD :: IOpr -> Duration -> Duration -> Duration
applyD op a b = case op of
  Add -> Duration $ ((duration a) + (duration b))
  Mul -> Duration $ ((duration a) * (duration b)) 
  Sub -> Duration $ ((duration a) - (duration b))
  Div -> Duration $ (duration a) `div` (duration b)
  Mod -> Duration $ (duration a) `mod` (duration b)

