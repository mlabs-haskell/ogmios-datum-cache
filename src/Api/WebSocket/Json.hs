{-# LANGUAGE DuplicateRecordFields #-}

module Api.WebSocket.Json (
    JsonWspResponse,
    JsonWspFault,
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
    , reflection :: Maybe Text
    }
    deriving stock (Generic)

instance ToJSON JsonWspResponse where
    toJSON (JsonWspResponse method res reflection) =
        object
            [ "type" .= ("jsonwsp/response" :: Text)
            , "version" .= ("1.0" :: Text)
            , "servicename" .= ("ogmios-datum-cache" :: Text)
            , "methodname" .= method
            , "result" .= res
            , "reflection" .= reflection
            ]

data JsonWspFault = JsonWspFault
    { methodname :: Text
    , faultCode :: Text
    , faultString :: Text
    , reflection :: Maybe Text
    }
    deriving stock (Generic)

instance ToJSON JsonWspFault where
    toJSON (JsonWspFault method code str reflection) =
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
            , "reflection" .= reflection
            ]

mkGetDatumByHashResponse :: Maybe PlutusData.Data -> Maybe Text -> JsonWspResponse
mkGetDatumByHashResponse = \case
    Just datumValue ->
        JsonWspResponse "GetDatumByHash" (object ["DatumFound" .= value])
      where
        value = object ["value" .= toJSON datumValue]
    Nothing ->
        JsonWspResponse "GetDatumByHash" (object ["DatumNotFound" .= Null])

mkGetDatumByHashFault :: Text -> Maybe Text -> JsonWspFault
mkGetDatumByHashFault =
    JsonWspFault "GetDatumByHash" "client"

mkGetDatumsByHashesResponse :: Maybe [Json.Value] -> Maybe Text -> JsonWspResponse
mkGetDatumsByHashesResponse = \case
    Just datumsWithValues ->
        JsonWspResponse "GetDatumsByHashes" (object ["DatumsFound" .= value])
      where
        value = object ["value" .= datumsWithValues]
    Nothing ->
        JsonWspResponse "GetDatumsByHashes" (object ["DatumsNotFound" .= Null])

mkGetDatumsByHashesFault :: Text -> Maybe Text -> JsonWspFault
mkGetDatumsByHashesFault =
    JsonWspFault "GetDatumsByHashes" "client"

mkGetBlockResponse :: BlockInfo -> Maybe Text -> JsonWspResponse
mkGetBlockResponse block = JsonWspResponse "GetBlock" (object ["block" .= block])

mkGetBlockFault :: Maybe Text -> JsonWspFault
mkGetBlockFault = JsonWspFault "GetBlock" "notFound" ""

mkStartFetchBlocksResponse :: Maybe Text -> JsonWspResponse
mkStartFetchBlocksResponse =
    JsonWspResponse "StartFetchBlocks" (object ["StartedBlockFetcher" .= Bool True])

mkStartFetchBlocksFault :: Text -> Maybe Text -> JsonWspFault
mkStartFetchBlocksFault =
    JsonWspFault "StartFetchBlocks" "client"

mkCancelFetchBlocksResponse :: Maybe Text -> JsonWspResponse
mkCancelFetchBlocksResponse =
    JsonWspResponse "CancelFetchBlocks" (object ["StoppedBlockFetcher" .= Bool True])

mkCancelFetchBlocksFault :: Text -> Maybe Text -> JsonWspFault
mkCancelFetchBlocksFault =
    JsonWspFault "CancelFetchBlocks" "client"
