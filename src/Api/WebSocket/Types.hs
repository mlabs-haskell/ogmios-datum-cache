module Api.WebSocket.Types (
  JsonWspRequest (JsonWspRequest),
  Method (..),
  GetDatumsByHashesDatum (..),
) where

import Data.Aeson (FromJSON, ToJSON, parseJSON, withObject, (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

import Block.Filter (DatumFilter)
import Block.Types (BlockInfo)
import PlutusData qualified

data JsonWspRequest = JsonWspRequest
  { mirror :: Maybe Aeson.Value
  , method :: Method
  }
  deriving stock (Generic, Show, Eq)

instance FromJSON JsonWspRequest where
  parseJSON = withObject "GetDatumByHash" $ \o ->
    JsonWspRequest <$> (o .:? "mirror") <*> parseMethod o
    where
      parseMethod o = do
        (method :: Text) <- o .: "methodname"
        case method of
          "GetDatumByHash" -> do
            args <- o .: "args"
            hash <- args .: "hash"
            pure $ GetDatumByHash hash
          "GetDatumsByHashes" -> do
            args <- o .: "args"
            hashes <- args .: "hashes"
            pure $ GetDatumsByHashes hashes
          "GetBlock" -> pure GetBlock
          "GetHealthcheck" -> do
            pure GetHealthcheck
          "SetStartingBlock" -> do
            args <- o .: "args"
            block <- args .: "startingBlock"
            pure $ SetStartingBlock block
          "SetDatumFilter" -> do
            args <- o .: "args"
            datumFilter <- args .: "datumFilter"
            pure $ SetDatumFilter datumFilter
          _ -> fail "Unexpected method"

data Method
  = GetDatumByHash Text
  | GetDatumsByHashes [Text]
  | GetBlock
  | GetHealthcheck
  | SetStartingBlock BlockInfo
  | SetDatumFilter DatumFilter
  deriving stock (Show, Eq)

data GetDatumsByHashesDatum = GetDatumsByHashesDatum
  { hash :: Text
  , value :: PlutusData.Data
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)
