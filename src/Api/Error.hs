module Api.Error (
  throwJsonError,
  JsonError (JsonError),
) where

import Control.Monad.Catch (MonadThrow, throwM)
import Data.Aeson (ToJSON (toJSON), encode, object, (.=))
import Data.String (IsString)
import Data.Text (Text)
import Network.HTTP.Types (hContentType)
import Servant (
  ServerError,
  errBody,
  errHeaders,
 )

newtype JsonError = JsonError
  { jsonError :: Text
  }
  deriving newtype (IsString)

instance ToJSON JsonError where
  toJSON err = object ["error" .= err.jsonError]

-- fourmolu is broken :(
{- ORMOLU_DISABLE -}
throwJsonError :: (MonadThrow m) => ServerError -> JsonError -> m b
throwJsonError err json =
  throwM err
    { errBody = encode json
    , errHeaders = [jsonHeader]
    }
  where
    jsonHeader = (hContentType, "application/json;charset=utf-8")
{- ORMOLU_ENABLE -}
