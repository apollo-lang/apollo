module Eval
( eval
) where
import Expr

eval :: Expr -> Expr
eval val@(ApolloInt      _) = val
eval val@(ApolloBool     _) = val
eval val@(ApolloDuration _) = val
eval val@(ApolloPitch    _) = val
eval (Cond tst csq alt)     = if checkBool $ eval tst
                              then eval csq
                              else eval alt
eval _ = error "evaluation error"

checkBool :: Expr -> Bool
checkBool (ApolloBool bool) = bool
checkBool x                 = error $ "expected Bool; instead: " ++ show x

