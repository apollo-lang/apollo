module Eval
( eval
) where
import Control.Monad (liftM)
import Expr
import Error

eval :: Expr -> ThrowsError Expr
eval expr = case expr of
  VInt i -> return $ VInt i

  VBool b -> return $ VBool b

  VList xs -> liftM VList (mapM eval xs)

  If test tr fl -> do
    VBool b <- eval test
    if b
    then eval tr
    else eval fl

  Not e -> do
    VBool b <- eval e
    return . VBool $ not b

  Neg e -> do
    VInt i <- eval e
    return . VInt $ -i

  IntOp op a b -> do
    VInt a' <- eval a
    VInt b' <- eval b
    return . VInt $ applyI op a' b'

  BoolOp op a b -> do
    VBool a' <- eval a
    VBool b' <- eval b
    return . VBool $ applyB op a' b'

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

