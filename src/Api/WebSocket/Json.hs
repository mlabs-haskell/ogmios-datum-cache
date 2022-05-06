{-# LANGUAGE DuplicateRecordFields #-}

module Api.WebSocket.Json (
    mkGetDatumByHashResponse,
    mkGetDatumByHashFault,
    mkGetDatumsByHashesResponse,
    mkGetDatumsByHashesFault,
    mkGetBlockResponse,
    mkGetBlockFault,
    mkStartFetchBlocksResponse,
    mkStartFetchBlocksFault,
    mkCancelFetchBlocksResponse,
    mkCancelFetchBlocksFault,
) where

import Data.Aeson
import Data.Aeson qualified as Json
import Data.Text (Text)
import GHC.Generics (Generic)

import Block.Types (BlockInfo)
import PlutusData qualified

-- {
--   type: 'jsonwsp/response',
--   version: '1.0',
--   servicename: 'ogmios',
--   methodname: 'FindIntersect',
--   result: { IntersectionFound: { point: [Object], tip: [Object] } },
--   reflection: null
-- }

data JsonWspResponse = JsonWspResponse
    { methodname :: Text
    , result :: Json.Value
    }
    deriving stock (Generic)

instance ToJSON JsonWspResponse where
    toJSON (JsonWspResponse method res) =
        object
            [ "type" .= ("jsonwsp/response" :: Text)
            , "version" .= ("1.0" :: Text)
            , "servicename" .= ("ogmios-datum-cache" :: Text)
            , "methodname" .= method
            , "result" .= res
            ]

data JsonWspFault = JsonWspFault
    { methodname :: Text
    , faultCode :: Text
    , faultString :: Text
    }
    deriving stock (Generic)

instance ToJSON JsonWspFault where
    toJSON (JsonWspFault method code str) =
        object
            [ "type" .= ("jsonwsp/fault" :: Text)
            , "version" .= ("1.0" :: Text)
            , "servicename" .= ("ogmios-datum-cache" :: Text)
            , "methodname" .= method
            , "fault"
                .= object
                    [ "code" .= code
                    , "string" .= str
                    ]
            ]

mkGetDatumByHashResponse :: Maybe PlutusData.Data -> JsonWspResponse
mkGetDatumByHashResponse = \case
    Just datumValue ->
        JsonWspResponse "GetDatumByHash" (object ["DatumFound" .= value])
      where
        value = object ["value" .= toJSON datumValue]
    Nothing ->
        JsonWspResponse "GetDatumByHash" (object ["DatumNotFound" .= Null])

mkGetDatumByHashFault :: Text -> JsonWspFault
mkGetDatumByHashFault =
    JsonWspFault "GetDatumByHash" "client"

mkGetDatumsByHashesResponse :: Maybe [Json.Value] -> JsonWspResponse
mkGetDatumsByHashesResponse = \case
    Just datumsWithValues ->
        JsonWspResponse "GetDatumsByHashes" (object ["DatumsFound" .= value])
      where
        value = object ["value" .= datumsWithValues]
    Nothing ->
        JsonWspResponse "GetDatumsByHashes" (object ["DatumsNotFound" .= Null])

mkGetDatumsByHashesFault :: Text -> JsonWspFault
mkGetDatumsByHashesFault =
    JsonWspFault "GetDatumsByHashes" "client"

mkGetBlockResponse :: BlockInfo -> JsonWspResponse
mkGetBlockResponse block = JsonWspResponse "GetBlock" (object ["block" .= block])

mkGetBlockFault :: JsonWspFault
mkGetBlockFault = JsonWspFault "GetBlock" "notFound" ""

mkStartFetchBlocksResponse :: JsonWspResponse
mkStartFetchBlocksResponse =
    JsonWspResponse "StartFetchBlocks" (object ["StartedBlockFetcher" .= Bool True])

mkStartFetchBlocksFault :: Text -> JsonWspFault
mkStartFetchBlocksFault =
    JsonWspFault "StartFetchBlocks" "client"

mkCancelFetchBlocksResponse :: JsonWspResponse
mkCancelFetchBlocksResponse =
    JsonWspResponse "CancelFetchBlocks" (object ["StoppedBlockFetcher" .= Bool True])

mkCancelFetchBlocksFault :: Text -> JsonWspFault
mkCancelFetchBlocksFault =
    JsonWspFault "CancelFetchBlocks" "client"
