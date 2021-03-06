
--------------------------------------------------------------------------
-- Eval: module for evaluating an Apollo expression
--------------------------------------------------------------------------

module Eval (
  eval
) where

import Control.Monad (liftM)
import Control.Monad.Error (throwError, liftIO)
import Data.IORef (newIORef, readIORef)
import Error
import Expr
import Env
import Type

eval :: Env Expr -> Expr -> IOThrowsError Expr
eval env expr = case expr of

  VInt i      -> return $ VInt i
  VBool b     -> return $ VBool b
  VPitch p    -> return $ VPitch (p `mod` 128)
  VDuration d -> return $ VDuration d

  VAtom a b -> do
    a' <- eval env a
    b' <- eval env b
    case (a', b') of
      (Nil, d)           -> return $ VAtom Nil (toVDuration d)
      (VList pitches, d) -> return $ VAtom (VList (map toVPitch pitches)) (toVDuration d)
      _                  -> return $ VAtom (toVPitch a') (toVDuration b')

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
      _        -> error "Error: expected Part or List"

  Tail l -> do
    l' <- eval env l
    case l' of
      VList (_:xs) -> return (VList xs)
      _            -> error "Error: expected Part or List"

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
        isZero _             = False

  ArrOp _ a l -> do
    a' <- eval env a
    l' <- eval env l
    case l' of
      VList ll -> return . VList $ a' : ll
      _        -> error "Error: expected Part or List"

  VList xs -> do
    xs' <- mapM (eval env) xs
    if pitchOrInt xs' 0
    then liftM VList (mapM (eval env . toVPitch) xs')
    else
      if durOrInt xs' 0
      then liftM VList (mapM (eval env . toVDuration) xs')
      else liftM VList (mapM (eval env) xs')

  Block body ret -> do
    env' <- clone' env
    mapM_ (eval env') body
    eval env' ret
      where
        clone' e = liftIO (readIORef e >>= newIORef . removeNames)
        removeNames = filter (\(n,_) -> n `notElem` names)
        names = map (\(Def name _ _) -> name) body

  VLam params body -> clone env >>= \closure ->
                      return (Function params body closure)

  VTLam _ _ params body -> clone env >>= \closure ->
                           return (Function params body closure)

  Def name TPitch ex -> do
    val <- eval env ex
    case val of
      VInt i   -> defineVar env name (VPitch (i `mod` 128)) >> return Empty
      VPitch p -> defineVar env name (VPitch (p `mod` 128)) >> return Empty
      _        -> return Empty

  Def name (TList TPitch) ex -> do
    val <- eval env ex
    case val of
      VList xs -> defineVar env name (VList (map toVPitch xs)) >> return Empty
      _        -> error "Error: expected List"

  Def name (TList TDuration) ex -> do
    val <- eval env ex
    case val of
        VList xs -> defineVar env name (VList (map toVDuration xs)) >> return Empty
        _        -> error "Error: expected List"

  Def name TDuration ex -> do
    val <- eval env ex
    case val of
      VInt i      -> defineVar env name (VDuration (nonneg i)) >> return Empty
      VDuration p -> defineVar env name (VDuration (nonneg p)) >> return Empty
      _           -> return Empty

  -- For recursion, binding names must be initialized
  -- before they are stored. (below)

  Def name _ ex@(VLam p b) -> do
    _ <- eval env ex
    _ <- defineVar env name Empty
    env' <- clone env
    _ <- setVar env' name (Function p b env')
    return Empty

  Def name _ ex -> eval env ex >>= defineVar env name >> return Empty

  Name name -> getVar env name

  FnCall (Name name) args -> do
    Function p b closure <- getVar env name
    args' <- mapM (eval env) args
    apply p b closure args'

  FnCall (VTLam _ _ is e) args -> do
    args' <- mapM (eval env) args
    apply is e env args'

  Nil -> return Nil

  other -> error $ "bug: `eval` called on " ++ show other

evalP :: Env Expr -> Expr -> IOThrowsError Expr
evalP env expr = case expr of
  VAtom a b  -> eval env $ VAtom a b
  Name name  -> getVar env name >>= evalP env
  _          -> throwError $ Default "Error: expected Note, Rest or Chord"

evalM :: Env Expr -> Expr -> IOThrowsError Expr
evalM env expr = case expr of
  VList p   -> liftM VList (mapM (evalP env) p)
  Name name -> getVar env name >>= evalM env
  _         -> throwError $ Default "Error: expected Part"

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

matchI _ _ _ = error "bug: matchI called with invalid operand types"

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
      createEnv = bindVars closure . zip params

toVPitch :: Expr -> Expr
toVPitch (VPitch p) = VPitch $ p `mod` 128
toVPitch (VInt i)   = VPitch $ i `mod` 128
toVPitch _          = error "Expected VInt or VPitch"

toVDuration :: Expr -> Expr
toVDuration (VDuration d) = VDuration d
toVDuration (VInt i)      = VDuration i
toVDuration _             = error "Expected VInt or VPitch"

nonneg :: Int -> Int
nonneg n | n >= 0    = n
         | otherwise = 0

pitchOrInt :: [Expr] -> Int -> Bool
pitchOrInt [] 0            = False
pitchOrInt [] _            = True
pitchOrInt (VPitch _:xs) n = pitchOrInt xs (n + 1)
pitchOrInt (VInt _:xs)   n = pitchOrInt xs n
pitchOrInt _ _             = False

durOrInt :: [Expr] -> Int -> Bool
durOrInt [] 0               = False
durOrInt [] _               = True
durOrInt (VDuration _:xs) n = durOrInt xs (n + 1)
durOrInt (VInt _:xs) n      = durOrInt xs n
durOrInt _ _                = False

