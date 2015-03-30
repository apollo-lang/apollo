module Eval
( eval
) where
import Expr

eval :: Expr -> Expr
eval val@(ApolloInt      _) = val
eval val@(ApolloBool     _) = val
eval val@(ApolloDuration _) = val
eval val@(ApolloPitch    _) = val
eval (Binary binExp)        = evalBinOp binExp
eval (Cond tst csq alt)     = if getBool $ eval tst
                              then eval csq
                              else eval alt
eval _ = error "evaluation error"

evalBinOp :: BinOp -> Expr
evalBinOp (Add e1 e2) = ApolloInt  $ (+)  (getInt  e1) (getInt  e2)
evalBinOp (Sub e1 e2) = ApolloInt  $ (-)  (getInt  e1) (getInt  e2)
evalBinOp (Mul e1 e2) = ApolloInt  $ (*)  (getInt  e1) (getInt  e2)
evalBinOp (Div e1 e2) = ApolloInt  $ div  (getInt  e1) (getInt  e2)
evalBinOp (Mod e1 e2) = ApolloInt  $ mod  (getInt  e1) (getInt  e2)
evalBinOp (Eq  e1 e2) = ApolloBool $ (==) (getBool e1) (getBool e2)
evalBinOp (NEq e1 e2) = ApolloBool $ (/=) (getBool e1) (getBool e2)
evalBinOp (Le  e1 e2) = ApolloBool $ (<)  (getBool e1) (getBool e2)
evalBinOp (Gr  e1 e2) = ApolloBool $ (>)  (getBool e1) (getBool e2)
evalBinOp (LEq e1 e2) = ApolloBool $ (<=) (getBool e1) (getBool e2)
evalBinOp (GEq e1 e2) = ApolloBool $ (>=) (getBool e1) (getBool e2)
evalBinOp (And e1 e2) = ApolloBool $ (&&) (getBool e1) (getBool e2)
evalBinOp (Or  e1 e2) = ApolloBool $ (||) (getBool e1) (getBool e2)

getInt :: Expr -> Int
getInt x = case (eval x) of
             (ApolloInt i) -> i
             notInt        -> error $ "expected ApolloInt; instead got " ++ show notInt

getBool :: Expr -> Bool
getBool x = case (eval x) of
              (ApolloBool b) -> b
              notBool        -> error $ "expected ApolloBool; instead got " ++ show notBool

