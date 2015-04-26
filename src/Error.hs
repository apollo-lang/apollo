module Error
    ( ApolloError (..)
    , ThrowsError
    , trapError
    , extractValue
    ) where

import Control.Monad.Error (Error(noMsg,strMsg), MonadError, catchError)
import Expr

type ThrowsError = Either ApolloError

data ApolloError
  = TypeMismatch String String Expr
  | UnboundVar String String
  | ArgMismatch String Integer [Expr]
  | RedefVar String
  | ParseErr String
  | Default String

instance Error ApolloError where
  noMsg  = Default "an error has occured"
  strMsg = Default

instance Show ApolloError where
  show (TypeMismatch  fn expected found) = "Invalid type: for " ++ fn ++ " expected " ++ expected ++ ", found " ++ typeOf found ++ " (" ++ showVal found ++ ")"
  show (UnboundVar           action var) = action ++ " an unbound variable: " ++ var
  show (ArgMismatch name expected found) = "Argument mismatch: for function " ++ name ++ " expected " ++ show expected ++ " arguments; found (" ++ commaDelimit found ++ ")"
    where commaDelimit = init . concatMap ((++ ",") . showVal)
  show (RedefVar                    var) = "Multiple declaration: redefining variable " ++ var
  show (ParseErr                    val) = "Parse error: unexpected " ++ val
  show (Default                     msg) = msg

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: Either ApolloError a -> a
extractValue (Right val) = val
extractValue (Left    _) = error "bug: extractValue called with Left"

