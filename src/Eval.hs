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

  PitchOp op a b -> do
    VPitch (Pitch a') <- eval env a
    VPitch (Pitch b') <- eval env b
    return . VPitch $ applyP op a' b'

  IntOp op a b -> do
    VInt a' <- eval env a
    VInt b' <- eval env b
    return . VInt $ applyI op a' b'

  DurOp op a b -> do
    VDuration (Duration a') <- eval env a
    VDuration (Duration b') <- eval env b
    return . VDuration $ applyD op a' b'


  Block body ret -> mapM_ (eval env) body >> eval env ret

  Def name typ ex -> defineVar env name (typ, ex)

  Name name -> getVar env name >>= eval env . snd

  FnCall name args -> do
    ((TFunc params _), body) <- getVar env name
    args' <- mapM (eval env) args
    apply name params body env args'

  VNote _  -> throwError $ Default "Error: Note not yet implemented"
  VChord _ -> throwError $ Default "Error: Chord not yet implemented"
  Empty    -> throwError $ Default "Error: eval called on Empty"


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

applyP :: IOpr -> Int -> Int -> Pitch
applyP op a b = case op of
  Add -> Pitch $ (a + b) `mod` 128
  Mul -> Pitch $ (a * b) `mod` 128
  Sub -> Pitch $ (a - b) `mod` 128
  Div -> Pitch $ (a `div` b)
  Mod -> Pitch $ (a `mod` b)

applyD :: IOpr -> Int -> Int -> Duration
applyD op a b = case op of
  Add -> Duration $ a + b
  Mul -> Duration $ a * b
  Sub -> Duration $ a - b
  Div -> Duration $ a `div` b  
  Mod -> Duration $ a `mod` b

