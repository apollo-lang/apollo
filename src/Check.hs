module Check (
  typecheck
) where

import Control.Monad.Error (throwError, liftIO)
import Data.IORef (newIORef, readIORef)

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

  VMusic m    -> do
    t <- mapM (typecheck env) m
    if null t
    then throwError $ TypeExcept "Music cannot be empty"
    else
      if uniform t && (head t) == (TList TAtom)
      then return TMusic
      else throwError $ TypeExcept "Music only takes lists of Atoms"

  VList xs -> do
    t <- mapM (typecheck env) xs
    if null t
      then return $ TListEmpty
    else 
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
    else 
      case t of 
        (TList _) -> return t
        _         -> throwError (TypeUMismatch "!" t)

  Neg e -> do
    t <- typecheck env e
    if t == TInt
    then return TInt
    else throwError (TypeUMismatch "-" t)
      

  Head l -> do
    tl <- typecheck env l
    case tl of 
      (TList t) -> return t
      _         -> throwError (TypeUMismatch "h@" tl)

  Tail l -> do
    tl <- typecheck env l
    case tl of 
      (TList _) -> return tl
      _         -> throwError (TypeUMismatch "t@" tl)

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
      (TList t, TList t') -> do
        if t == t'
        then
          if op == Eq || op == NEq
          then return $ TList t
          else throwError (TypeMismatch (show op) ta tb)
        else throwError (TypeMismatch (show op) ta tb)
      (TList t, TListEmpty) -> return $ TList t
      (TListEmpty, TList t) -> return $ TList t
      (TListEmpty, TListEmpty) -> return TListEmpty
      _              -> throwError (TypeMismatch (show op) ta tb)

  IntOp op a b -> do
    ta <- typecheck env a
    tb <- typecheck env b
    case (ta, tb) of
      (TInt, TInt) -> return TInt
      _            -> throwError (TypeMismatch (show op) ta tb)

  ArrOp op a l -> do
    ta <- typecheck env a 
    tl <- typecheck env l
    case tl of 
      (TList t) -> do
        if ta == t
        then return $ TList ta
        else throwError (TypeMismatch (show op) ta (TList t))
      TListEmpty -> do
        return $ TList ta

      _ -> throwError $ TypeExcept "Expected list"
    

  Block body ret -> do
    env' <- clone env
    mapM_ (typecheck env') body
    typecheck env' ret
      where
        clone e = liftIO (readIORef e >>= newIORef . removeNames)
        removeNames = filter (\(n,_) -> n `notElem` names)
        names = map (\(Def name _ _) -> name) body

  Def name (TFunc p r) body -> return TError -- TODO

  Def name t ex -> do
    t' <- typecheck env ex
    if t == t' || t == TMusic && t' == (TList $ TList TAtom)
    then defineVar env name t
    else throwError (TypeDMismatch t t')

  Name name -> getVar env name

  other -> return (TEmpty (show other)) -- error $ "ERR: got: " ++ show other

uniform :: Eq a => [a] -> Bool
uniform ys = all (== head ys) ys



