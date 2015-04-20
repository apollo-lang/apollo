module Error
( ApolloError (..)
, ThrowsError
, extractValue
, trapError
) where
import Control.Monad.Error
import Expr

type ThrowsError = Either ApolloError

data ApolloError
  = TypeMismatch String Expr
  | UnboundVar String String
  | RedefVar String
  | ParseErr String
  | Default String

instance Error ApolloError where
  noMsg  = Default "an error has occured"
  strMsg = Default

instance Show ApolloError where
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ typeOf found ++ " (" ++ show found ++ ")"
  show (UnboundVar       action var) = action ++ " an unbound variable: " ++ var
  show (RedefVar                var) = "Multiple declaration: redefining variable " ++ var
  show (ParseErr                val) = "Parse error: unexpected token " ++ show val
  show (Default                 msg) = msg

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: Either ApolloError a -> a
extractValue (Right val) = val
extractValue (Left    _) = error "bug: extractValue called with Left"

