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

  BoolOp op a b -> do
    VBool a' <- eval env a
    VBool b' <- eval env b
    return . VBool $ applyB op a' b'

  IntOp op a@(VInt i) b -> do
    VInt a' <- eval env a
    VInt b' <- eval env b
    return . VInt $ applyI op a' b'

  PitchOp op a@(VPitch p) b -> do
    VPitch a' <- eval env a
    VPitch b' <- eval env b
    return . VPitch $ applyP op a' b'

  DurOp op a@(VDuration d) b -> do
    VDuration a' <- eval env a
    VDuration b' <- eval env b
    return . VDuration $ applyD op a' b'

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
  Add -> Pitch $ ((pitchInt a) + (pitchInt b)) `mod` 128
  Mul -> Pitch $ ((pitchInt a) * (pitchInt b)) `mod` 128
  Sub -> Pitch $ ((pitchInt a) - (pitchInt b)) `mod` 128
  Div -> Pitch $ (pitchInt a) `div` (pitchInt b)
  Mod -> Pitch $ (pitchInt a) `mod` (pitchInt b)

applyD :: IOpr -> Duration -> Duration -> Duration
applyD op a b = case op of
  Add -> Duration $ ((durationInt a) + (durationInt b))
  Mul -> Duration $ ((durationInt a) * (durationInt b)) 
  Sub -> Duration $ ((durationInt a) - (durationInt b))
  Div -> Duration $ (durationInt a) `div` (durationInt b)
  Mod -> Duration $ (durationInt a) `mod` (durationInt b)

