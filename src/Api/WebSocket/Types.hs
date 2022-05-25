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
import Block.Types (BlockInfo (BlockInfo))
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
          "StartFetchBlocks" -> do
            args <- o .: "args"
            slot <- args .: "slot"
            blockId <- args .: "id"
            datumFilter <- args .:? "datumFilter"
            pure $ StartFetchBlocks (BlockInfo slot blockId) datumFilter
          "CancelFetchBlocks" -> do
            pure CancelFetchBlocks
          "GetHealthcheck" -> do
            pure GetHealthcheck
          _ -> fail "Unexpected method"

data Method
  = GetDatumByHash Text
  | GetDatumsByHashes [Text]
  | GetBlock
  | StartFetchBlocks BlockInfo (Maybe DatumFilter)
  | CancelFetchBlocks
  | GetHealthcheck
  deriving stock (Show, Eq)

data GetDatumsByHashesDatum = GetDatumsByHashesDatum
  { hash :: Text
  , value :: PlutusData.Data
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)
