module Api.WebSocket.Types (JsonWspRequest (JsonWspRequest), Method (..), GetDatumsByHashesDatum (..)) where

import Data.Aeson (FromJSON, ToJSON, Value, parseJSON, withObject, (.:), (.:?))
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)

import Block.Filter (DatumFilter)
import PlutusData qualified

data JsonWspRequest = JsonWspRequest
    { mirror :: Maybe Value
    , method :: Method
    }
    deriving stock (Generic)

-- instance FromJSON JsonWspRequest where
--     parseJSON = withObject "JsonWspRequest" $ \o ->
--         JsonWspRequest <$> (o .:? "mirror") <*> parseMethod o
--       where
--         parseMethod o = do
--             (method :: Text) <- o .: "methodname"
--             case method of
--                 "GetDatumByHash" -> do
--                     args <- o .: "args"
--                     hash <- args .: "hash"
--                     pure $ GetDatumByHash hash
--                 "GetDatumsByHashes" -> do
--                     args <- o .: "args"
--                     hashes <- args .: "hashes"
--                     pure $ GetDatumsByHashes hashes
--                 "StartFetchBlocks" -> do
--                     args <- o .: "args"
--                     slot <- args .: "slot"
--                     blockId <- args .: "id"
--                     pure $ StartFetchBlocks slot blockId
--                 "CancelFetchBlocks" -> do
--                     pure CancelFetchBlocks
--                 "DatumFilterAddHashes" -> do
--                     args <- o .: "args"
--                     hashes <- args .: "hashes"
--                     pure $ DatumFilterAddHashes hashes
--                 "DatumFilterRemoveHashes" -> do
--                     args <- o .: "args"
--                     hashes <- args .: "hashes"
--                     pure $ DatumFilterRemoveHashes hashes
--                 "DatumFilterSetHashes" -> do
--                     args <- o .: "args"
--                     hashes <- args .: "hashes"
--                     pure $ DatumFilterSetHashes hashes
--                 "DatumFilterGetHashes" -> do
--                     pure DatumFilterGetHashes
--                 _ -> fail "Unexpected method"

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
                    datumFilter <- args .: "datumFilter"
                    pure $ StartFetchBlocks slot blockId datumFilter
                "CancelFetchBlocks" -> do
                    pure CancelFetchBlocks
                _ -> fail "Unexpected method"

data Method
    = GetDatumByHash Text
    | GetDatumsByHashes [Text]
    | GetBlock
    | StartFetchBlocks Int64 Text DatumFilter
    | CancelFetchBlocks
    deriving stock (Show)

data GetDatumsByHashesDatum = GetDatumsByHashesDatum
    { hash :: Text
    , value :: PlutusData.Data
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON)
