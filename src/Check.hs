module Check (
  typecheck
) where

import Control.Monad.Error (throwError, liftIO, liftM)
import Data.IORef (newIORef, readIORef)

import Error
import Type
import Expr
import Env

typecheck :: Env Type -> Expr -> IOThrowsError Type
typecheck env expr = case expr of

  VInt{}      -> return TInt
  VBool{}     -> return TBool
  VPitch{}    -> return TPitch
  VDuration{} -> return TDuration

  VAtom a b     -> do
    ta <- typecheck env a
    tb <- typecheck env b
    if (ta == TNil || ta == TInt || ta == TPitch || ta == TList TPitch || ta == TList TInt )
      && (tb == TInt || tb == TDuration)
    then return TAtom
    else throwError $ TypeExcept "Atom must contain Pitch and Duration"

  VMusic m    -> do
    t <- mapM (typecheck env) m
    if null t
    then throwError $ TypeExcept "Music cannot be empty"
    else
      if uniform t && head t == TList TAtom
      then return TMusic
      else throwError $ TypeExcept "Music only takes lists of Atoms"

  VList xs -> do
    t <- mapM (typecheck env) xs
    if null t
      then return TListEmpty
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
        if t1 == t2 || bothList t1 t2
           then return (isNotEmptyOf t1 t2)
           else throwError $ TypeExcept "If: case mismatch"
      _    -> throwError $ TypeExcept "If: bool-cond not bool"
   where
     bothList TList{} TListEmpty = True
     bothList TListEmpty TList{} = True
     bothList _ _                = False
     isNotEmptyOf TListEmpty x = x
     isNotEmptyOf x TListEmpty = x
     isNotEmptyOf x _ = x

  Not e -> do
    t <- typecheck env e
    if t == TBool || isList t
    then return TBool
    else
      case t of
        (TList _) -> return t
        _         -> throwError (TypeUMismatch "!" t)
   where
    isList TListEmpty = True
    isList (TList _)  = True
    isList _          = False

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
      (TInt, TInt)   -> return TBool
      (TPitch, TPitch)   -> return TBool
      (TDuration, TDuration)   -> return TBool
      (TBool, TBool) -> return TBool
      (TList t, TList t') ->
        if (t == t') && (op == Eq || op == NEq)
        then return TBool
        else throwError (TypeMismatch (show op) ta tb)
      (TList _, TListEmpty) -> return TBool
      (TListEmpty, TList _) -> return TBool
      (TListEmpty, TListEmpty) -> return TListEmpty
      _              -> throwError (TypeMismatch (show op) ta tb)

  IntOp op a b -> do
    ta <- typecheck env a
    tb <- typecheck env b
    case (ta, tb) of
      (TInt, TInt) -> return TInt
      (TPitch, TInt) ->
        if op == Add || op == Sub
        then return TPitch
        else throwError (TypeMismatch (show op) ta tb)
      (TInt, TPitch) ->
        if op == Add
        then return TPitch
        else throwError (TypeMismatch (show op) ta tb)
      (TInt, TDuration) ->
        if op == Mul || op == Div
        then return TDuration
        else throwError (TypeMismatch (show op) ta tb)
      (TDuration, TInt) ->
        if op == Mul || op == Div
        then return TDuration
        else throwError (TypeMismatch (show op) ta tb)
      (TDuration, TDuration) ->
        if op == Add || op == Sub
        then return TDuration
        else throwError (TypeMismatch (show op) ta tb)

      _            -> throwError (TypeMismatch (show op) ta tb)

  ArrOp op a l -> do
    ta <- typecheck env a
    tl <- typecheck env l
    case tl of
      (TList t) ->
        if ta == t
        then return $ TList ta
        else throwError (TypeMismatch (show op) ta (TList t))
      TListEmpty -> return (TList ta)

      _ -> throwError $ TypeExcept ("Expected list; got " ++ show tl)

  Block body ret -> do
    env' <- clone' env
    mapM_ (typecheck env') body
    typecheck env' ret
      where
        clone' e = liftIO (readIORef e >>= newIORef . removeNames)
        removeNames = filter (\(n,_) -> n `notElem` names)
        names = map (\(Def name _ _) -> name) body

  -- TODO NOTE: had to work around fact that fn types aren't a single value here vs elsewhere

  FnCall (Name name) args -> do
    (TFunc tps tr) <- getVar env name
    if length tps /= length args
    then throwError . Default $ "arg count mismatch for " ++ name
    else do
      ta <- mapM (check env) (zip tps args)
      if ta == tps
      then return tr
      else throwError . Default $ "expected args: " ++ show tps ++ "; actual: " ++ show ta ++ " for " ++ name
   where
     check _ (param, arg) = if isTFunc param && isName arg
                            then getVar env (getName arg)
                            else typecheck env arg
     isTFunc TFunc{}    = True
     isTFunc _          = False
     isName Name{}      = True
     isName _           = False
     getName (Name a)   = a
     getName _          = ""

  -- TODO: check param count on call; this is currently done, but should we add another `if` to make it more explicit?

  Def name t@(TFunc pTypes rType) (VLam params body) -> do
    _ <- defineVar env name t  -- done first so recusion works
    e <- bindVars env (zip params pTypes)
    r <- typecheck e body
    if rType == r
    then return TEmpty
    else throwError (TypeRMismatch name rType t)

  Def name t ex -> do
    t' <- typecheck env ex
    if t == t' || t == TMusic && t' == (TList $ TList TAtom) || match t t'
    then defineVar env name t
    else throwError (TypeDMismatch t t')

  Name name -> liftM getType (getVar env name)
    where
      getType (TFunc _ returnType) = returnType
      getType other                = other

  VTLam pTypes rType params body -> do
    e <- bindVars env (zip params pTypes)
    r <- typecheck e body
    if rType == r
    then return (TFunc pTypes rType)
    else throwError (TypeRMismatch "<lambda>" rType r)

  Nil       -> return TNil

  other -> return (TErrVerbose (show other)) -- error $ "ERR: got: " ++ show other


uniform :: Eq a => [a] -> Bool
uniform ys = all (== head ys) ys


match :: Type -> Type -> Bool
match a b = case (a, b) of
        (TPitch, TInt)    -> True
        (TDuration, TInt) -> True
        _                 -> False

