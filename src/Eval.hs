module Eval
( eval
) where
import Control.Monad.Error (throwError)
import Control.Monad (liftM)
import Expr
import Error

eval :: Expr -> ThrowsError Expr
eval val@(ApolloInt      _) = return val
eval val@(ApolloBool     _) = return val
eval (Unary      unExp)     = evalUnOp unExp
eval (Binary    binExp)     = evalBinOp binExp
eval (ApolloList    xs)     = liftM ApolloList (mapM eval xs)
eval (Cond tst csq alt)     = getBool tst >>= (\b -> eval $ if b then csq else alt)
eval _                      = error "TODO: evaluation error"

evalUnOp :: UnOp -> ThrowsError Expr
evalUnOp (Not e) = liftM (ApolloBool . not) (getBool e)
evalUnOp (Neg e) = liftM (ApolloInt  . neg) (getInt  e)
  where neg i = -i

evalBinOp :: BinOp -> ThrowsError Expr
evalBinOp (Add e1 e2) = applyI (+)  e1 e2
evalBinOp (Sub e1 e2) = applyI (-)  e1 e2
evalBinOp (Mul e1 e2) = applyI (*)  e1 e2
evalBinOp (Div e1 e2) = applyI div  e1 e2
evalBinOp (Mod e1 e2) = applyI mod  e1 e2
evalBinOp (Eq  e1 e2) = applyB (==) e1 e2
evalBinOp (NEq e1 e2) = applyB (/=) e1 e2
evalBinOp (Le  e1 e2) = applyB (<)  e1 e2
evalBinOp (Gr  e1 e2) = applyB (>)  e1 e2
evalBinOp (LEq e1 e2) = applyB (<=) e1 e2
evalBinOp (GEq e1 e2) = applyB (>=) e1 e2
evalBinOp (And e1 e2) = applyB (&&) e1 e2
evalBinOp (Or  e1 e2) = applyB (||) e1 e2

applyI :: (Int -> Int -> Int) -> Expr -> Expr -> ThrowsError Expr
applyI op e1 e2 = do
  i1 <- getInt e1
  i2 <- getInt e2
  return . ApolloInt $ op i1 i2

applyB :: (Bool -> Bool -> Bool) -> Expr -> Expr -> ThrowsError Expr
applyB op e1 e2 = do
  b1 <- getBool e1
  b2 <- getBool e2
  return . ApolloBool $ op b1 b2

getInt :: Expr -> ThrowsError Int
getInt x = do
  e <- eval x
  case e of
    (ApolloInt i) -> return i
    notInt        -> throwError $ TypeMismatch "ApolloInt" notInt

getBool :: Expr -> ThrowsError Bool
getBool x = do
  e <- eval x
  case e of
    (ApolloBool b) -> return b
    notBool        -> throwError $ TypeMismatch "ApolloBool" notBool

