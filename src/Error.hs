module Error
( ApolloError (..)
, ThrowsError
, extractValue
, trapError
) where
import Control.Monad.Error
import Data.Typeable (typeOf)
import Expr (Expr (..))

type ThrowsError = Either ApolloError

data ApolloError
  = TypeMismatch String Expr
  | Default String

instance Error ApolloError where
  noMsg  = Default "an error has occured"
  strMsg = Default

instance Show ApolloError where
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show (typeOf found) ++ " (" ++ show found ++ ")"
  show (Default  msg) = msg

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: Either ApolloError a -> a
extractValue (Right val) = val
extractValue (Left    _) = error "bug: `extractValue` called with Left"

