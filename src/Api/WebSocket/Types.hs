module Api.WebSocket.Types where

import Data.Text (Text)
import GHC.Generics (Generic)

import Data.Aeson

data Method =
    GetDatumByHash Text
  | GetDatumsByHashes [Text]
  deriving stock Show

instance FromJSON Method where
  parseJSON = withObject "GetDatumByHash" $ \o -> do
    (method :: Text) <- o .: "methodname"
    case method of
      "GetDatumByHash" -> do
        args <- o .: "args"
        hash <- args .: "hash"
        pure $ GetDatumByHash hash
      "GetDatumsByHashes" -> do
        pure $ GetDatumsByHashes ["1"]
      _ -> fail ""
    -- pure $ GetDatumByHash ""
