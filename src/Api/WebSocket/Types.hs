module Api.WebSocket.Types (
  JsonWspRequest (JsonWspRequest),
  Method (..),
  GetDatumsByHashesDatum (..),
) where

import Data.Aeson (FromJSON, ToJSON, parseJSON, withObject, (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

import App.Env (ControlApiToken (ControlApiToken))
import Block.Filter (DatumFilter)
import Block.Types (StartingBlock)
import DataHash (DataHash (DataHash))
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
            pure $ GetDatumByHash (DataHash hash)
          "GetDatumsByHashes" -> do
            args <- o .: "args"
            hashes <- args .: "hashes"
            pure $ GetDatumsByHashes (DataHash <$> hashes)
          "GetTxByHash" -> do
            args <- o .: "args"
            hash <- args .: "hash"
            pure $ GetTxByHash hash
          "GetBlock" -> pure GetBlock
          "GetHealthcheck" -> do
            pure GetHealthcheck
          "SetStartingBlock" -> do
            args <- o .: "args"
            block <- args .: "startingBlock"
            token <- ControlApiToken <$> args .: "token"
            pure $ SetStartingBlock token block
          "SetDatumFilter" -> do
            args <- o .: "args"
            datumFilter <- args .: "datumFilter"
            token <- ControlApiToken <$> args .: "token"
            pure $ SetDatumFilter token datumFilter
          _ -> fail "Unexpected method"

data Method
  = GetDatumByHash DataHash
  | GetDatumsByHashes [DataHash]
  | GetTxByHash Text
  | GetBlock
  | GetHealthcheck
  | SetStartingBlock ControlApiToken StartingBlock
  | SetDatumFilter ControlApiToken DatumFilter
  deriving stock (Show, Eq)

data GetDatumsByHashesDatum = GetDatumsByHashesDatum
  { hash :: DataHash
  , value :: PlutusData.Data
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)
