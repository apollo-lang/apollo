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
  VPart{}     -> return TPart
  VMusic{}    -> return TMusic

  VList xs -> do
    t <- mapM (typecheck env) xs
    if null t
      then return $ TList TInt  -- TODO: FIX THIS, TInt is a placeholder
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
        TPart     -> return t   --TODO: We can't have a Part || since this is OR
                                --so ! is useless right now
        _         -> throwError (TypeUMismatch "!" t)

  Neg e -> do
    t <- typecheck env e
    if t == TInt
    then return TInt
    else throwError (TypeUMismatch "-" t)
      

  Head l -> do
    tl <- typecheck env l
    case tl of 
      (TList _) -> return tl
      TPart     -> return tl
      _         -> throwError (TypeUMismatch "h@" tl)

  Tail l -> do
    tl <- typecheck env l
    case tl of 
      (TList _) -> return tl
      TPart     -> return tl
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
      TPart     -> do
        if ta == TAtom
        then return $ TPart
        else throwError (TypeMismatch (show op) ta TPart)

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
    if t == t'
    then defineVar env name t
    else throwError (TypeDMismatch t t')

  Name name -> getVar env name

  other -> return (TEmpty (show other)) -- error $ "ERR: got: " ++ show other

uniform :: Eq a => [a] -> Bool
uniform ys = all (== head ys) ys

