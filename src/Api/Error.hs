module Api.Error (
    throwJsonError,
    JsonError (..),
) where

import Control.Monad.Catch (MonadThrow, throwM)
import Data.Aeson (ToJSON, encode)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Types (hContentType)
import Servant (
    ServerError,
    errBody,
    errHeaders,
 )

newtype JsonError = JsonError
    { error :: Text
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON)

throwJsonError :: (MonadThrow m, ToJSON a) => ServerError -> a -> m b
throwJsonError err json =
    throwM
        err
            { errBody = encode json
            , errHeaders = [jsonHeader]
            }
  where
    jsonHeader = (hContentType, "application/json;charset=utf-8")
