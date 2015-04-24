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

  CompOp op a b -> do
    a' <- eval env a
    b' <- eval env b
    return . VBool $ applyC op a' b'

  Block body ret -> mapM_ (eval env) body >> eval env ret

  Def name typ ex -> defineVar env name (typ, ex)

  Name name -> getVar env name >>= eval env . snd

  -- TODO: should `id` be a Name type
  -- TODO: typecheck
  -- TODO: before typecheck, check type is TFunc to be safe? or is there a more concise way to be safe about this?
  -- TODO: does closure need more than just env?

  FnCall name args -> do
    ((TFunc params typ), body) <- getVar env name
    args' <- mapM (eval env) args
    apply name params body env args'

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

apply :: String -> [Param] -> Expr -> Env -> [Expr] -> IOThrowsError Expr
apply name paramList body closure args =
  if num params /= num args
  then throwError $ ArgMismatch name (num params) args
  else createEnv args >>= (flip eval) body
    where num = toInteger . length
          params = map (\(Param n _) -> n) paramList
          createEnv = liftIO . bindVars closure . zip params . map (\a -> (TData "TODO", a))

