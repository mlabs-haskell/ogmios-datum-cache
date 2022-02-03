module Api.WebSocket.Types (Method (..), GetDatumsByHashesDatum (..)) where

import Data.Aeson (FromJSON, ToJSON, parseJSON, withObject, (.:))
import Data.Text (Text)
import GHC.Generics (Generic)

import PlutusData qualified

data Method
    = GetDatumByHash Text
    | GetDatumsByHashes [Text]
    | StartFetchBlocks Integer Text
    | CancelFetchBlocks
    | DatumFilterAddHashes [Text]
    | DatumFilterRemoveHashes [Text]
    | DatumFilterSetHashes [Text]
    | DatumFilterGetHashes
    deriving stock (Show)

instance FromJSON Method where
    parseJSON = withObject "GetDatumByHash" $ \o -> do
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
            "StartFetchBlocks" -> do
                args <- o .: "args"
                slot <- args .: "slot"
                blockId <- args .: "id"
                pure $ StartFetchBlocks slot blockId
            "CancelFetchBlocks" -> do
                pure CancelFetchBlocks
            "DatumFilterAddHashes" -> do
                args <- o .: "args"
                hashes <- args .: "hashes"
                pure $ DatumFilterAddHashes hashes
            "DatumFilterRemoveHashes" -> do
                args <- o .: "args"
                hashes <- args .: "hashes"
                pure $ DatumFilterRemoveHashes hashes
            "DatumFilterSetHashes" -> do
                args <- o .: "args"
                hashes <- args .: "hashes"
                pure $ DatumFilterSetHashes hashes
            "DatumFilterGetHashes" -> do
                pure DatumFilterGetHashes
            _ -> fail "Unexpected method"

data GetDatumsByHashesDatum = GetDatumsByHashesDatum
    { hash :: Text
    , value :: PlutusData.Data
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON)
