module Check (
  typecheck
) where

import Control.Monad.Error (Error(noMsg,strMsg), throwError)
import qualified Data.Map as Map

import Expr hiding (Type, Pitch, Duration, Rest, Chord, Empty)
import Error

data Type = Int | Bool | Pitch | Duration | Rest | Note | Chord | List Type | Empty
  deriving (Eq, Show)

typecheck :: Expr -> ThrowsError Type
typecheck expr = case expr of

  VInt{}      -> return Int
  VBool{}     -> return Bool
  VPitch{}    -> return Pitch
  VDuration{} -> return Duration
  VRest{}     -> return Rest
  VChord{}    -> return Chord

  VList xs -> do
    t <- mapM typecheck xs
    if uniform t
    then return $ List (head t)
    else throwError $ TypeExcept "list is irregular"

  If test tr fl -> do
    t <- typecheck test
    case t of
      Bool -> do
        t1 <- typecheck tr
        t2 <- typecheck fl
        if t1 == t2
        then return t1
        else throwError $ TypeExcept "If: case mismatch"
      _    -> throwError $ TypeExcept "If: bool-cond not bool"

  Not e -> do
    t <- typecheck e
    if t == Bool
    then return Bool
    else throwError $ TypeExcept "Not: expected Bool"

  Neg e -> do
    t <- typecheck e
    if t == Int
    then return Int
    else throwError $ TypeExcept "Neg: expected Int"

  BoolOp op a b -> do
    ta <- typecheck a
    tb <- typecheck b
    case (ta, tb) of
      (Bool, Bool) -> return Bool
      _            -> throwError $ TypeExcept "mismatch"

  CompOp op a b -> do
    ta <- typecheck a
    tb <- typecheck b
    case (ta, tb) of
      (Int, Int)   -> return Int
      (Bool, Bool) -> return Bool
      _            -> throwError $ TypeExcept "mismatch"

  IntOp op a b -> do
    ta <- typecheck a
    tb <- typecheck b
    case (ta, tb) of
      (Int, Int) -> return Int
      _          -> throwError $ TypeExcept "mismatch"

  Block body ret -> do
    mapM_ typecheck body
    typecheck ret

  other -> return Empty -- error $ "ERR: got: " ++ show other

uniform :: Eq a => [a] -> Bool
uniform ys = all (== head ys) ys

