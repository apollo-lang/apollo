module Eval
( eval
) where
import Control.Monad.Error (throwError)
import Control.Monad (liftM)
import Expr
import Error

eval :: Expr -> ThrowsError Expr
eval val@(ApolloInt  _) = return val
eval val@(ApolloBool _) = return val
eval (ApolloList    xs) = liftM ApolloList (mapM eval xs)
eval (Cond tst csq alt) = getBool tst >>= (\b -> eval $ if b then csq else alt)

-- Unary operations:

eval (Not e) = liftM (ApolloBool . not) (getBool e)
eval (Neg e) = liftM (ApolloInt  . neg) (getInt  e)
  where neg i = -i

-- Binary operations:

eval (Add e1 e2) = applyI (+)  e1 e2
eval (Sub e1 e2) = applyI (-)  e1 e2
eval (Mul e1 e2) = applyI (*)  e1 e2
eval (Div e1 e2) = applyI div  e1 e2
eval (Mod e1 e2) = applyI mod  e1 e2
eval (Eq  e1 e2) = applyB (==) e1 e2
eval (NEq e1 e2) = applyB (/=) e1 e2
eval (Le  e1 e2) = applyB (<)  e1 e2
eval (Gr  e1 e2) = applyB (>)  e1 e2
eval (LEq e1 e2) = applyB (<=) e1 e2
eval (GEq e1 e2) = applyB (>=) e1 e2
eval (And e1 e2) = applyB (&&) e1 e2
eval (Or  e1 e2) = applyB (||) e1 e2

-- Handle case for not-yet-implemented types:

eval _                      = error "evaluation error: not yet implemented"

-- Helper functions:

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

