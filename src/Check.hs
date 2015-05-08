module Check (
  typecheck
) where

import Control.Monad.Error (throwError)

import Error
import Expr
import Env

typecheck :: Env Type -> Expr -> IOThrowsError Type
typecheck env expr = case expr of

  VInt{}      -> return TInt
  VBool{}     -> return TBool
  VPitch{}    -> return TPitch
  VDuration{} -> return TDuration
  VAtom{}     -> return TAtom
  VPart{}     -> return TPart
  VMusic{}    -> return TMusic
  
  VList xs -> do
    t <- mapM (typecheck env) xs
    if uniform t
    then return $ TList (head t)
    else throwError $ TypeExcept "list is irregular"

  If test tr fl -> do
    t <- typecheck env test
    case t of
      TBool -> do
        t1 <- typecheck env tr
        t2 <- typecheck env fl
        if t1 == t2
        then return t1
        else throwError $ TypeExcept "If: case mismatch"
      _    -> throwError $ TypeExcept "If: bool-cond not bool"

  Not e -> do
    t <- typecheck env e
    if t == TBool
    then return TBool
    else throwError (TypeUMismatch "!" t)

  Neg e -> do
    t <- typecheck env e
    if t == TInt
    then return TInt
    else throwError (TypeUMismatch "-" t)

  BoolOp op a b -> do
    ta <- typecheck env a
    tb <- typecheck env b
    case (ta, tb) of
      (TBool, TBool) -> return TBool
      _              -> throwError (TypeMismatch (show op) ta tb)

  CompOp op a b -> do
    ta <- typecheck env a
    tb <- typecheck env b
    case (ta, tb) of
      (TInt, TInt)   -> return TInt
      (TBool, TBool) -> return TBool
      _              -> throwError (TypeMismatch (show op) ta tb)

  IntOp op a b -> do
    ta <- typecheck env a
    tb <- typecheck env b
    case (ta, tb) of
      (TInt, TInt) -> return TInt
      _            -> throwError (TypeMismatch (show op) ta tb)

  Block body ret -> do
    mapM_ (typecheck env) body
    typecheck env ret

  Def name (TFunc p r) body -> return TError -- TODO

  Def name t ex -> do
    t' <- typecheck env ex
    if t == t'
    then defineVar env name t
    else throwError (TypeDMismatch t t')

  Name name -> getVar env name

  other -> return (TEmpty (show other)) -- error $ "ERR: got: " ++ show other

uniform :: Eq a => [a] -> Bool
uniform ys = all (== head ys) ys

