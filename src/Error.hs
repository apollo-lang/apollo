module Error
    ( ApolloError (..)
    , ThrowsError
    , trapError
    , extractValue
    ) where
import Control.Monad.Error (Error(noMsg,strMsg), MonadError, catchError)

import Type

type ThrowsError = Either ApolloError

data ApolloError
  = TypeMismatch String Type Type
  | TypeDMismatch Type Type
  | TypeUMismatch String Type
  | TypeRMismatch String Type Type
  | TypeExcept String
  | UnboundVar String String
  | RedefVar String
  | ParseErr String
  | DivByZero
  | Default String

instance Error ApolloError where
  noMsg  = Default "an error has occured"
  strMsg = Default

instance Show ApolloError where
  show (TypeMismatch  op a b)  = "Type error: " ++ show a ++ " and " ++ show b ++ " are wrong operand types for `" ++ op ++ "`"
  show (TypeUMismatch op a)    = "Type error: " ++ show a ++ " is wrong operand type for unary `" ++ op ++ "`"
  show (TypeDMismatch a b)     = "Type error: definition of " ++ show a ++ ", but assigned to " ++ show b
  show (TypeRMismatch f a b)   = "Type error: `" ++ f ++ "` defined with return-type of " ++ show a ++ ", but actual return-type is " ++ show b
  show (TypeExcept msg)        = "Type error: " ++ show msg
  show (UnboundVar action var) = action ++ " an unbound variable: " ++ var
  show (RedefVar var)          = "Multiple declaration: redefining variable " ++ var
  show (ParseErr val)          = "Parse error: unexpected " ++ val
  show (DivByZero)             = "Zero-division error: division or modulo by zero"
  show (Default msg)           = msg

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: Either ApolloError a -> a
extractValue (Right val) = val
extractValue (Left    _) = error "bug: extractValue called with Left"

