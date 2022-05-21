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
  mkHealthcheckResponse,
) where

import Data.Aeson
    ( ToJSON(toJSON), object, Value(Bool, Null), KeyValue((.=)) )
import Data.Aeson qualified as Aeson
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
  , result :: Aeson.Value
  , reflection :: Maybe Aeson.Value
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
  , reflection :: Maybe Aeson.Value
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

mkGetDatumByHashResponse :: Maybe PlutusData.Data -> Maybe Aeson.Value -> JsonWspResponse
mkGetDatumByHashResponse = \case
  Just datumValue ->
    JsonWspResponse "GetDatumByHash" (object ["DatumFound" .= value])
    where
      value = object ["value" .= toJSON datumValue]
  Nothing ->
    JsonWspResponse "GetDatumByHash" (object ["DatumNotFound" .= Null])

mkGetDatumByHashFault :: Text -> Maybe Aeson.Value -> JsonWspFault
mkGetDatumByHashFault =
  JsonWspFault "GetDatumByHash" "client"

mkGetDatumsByHashesResponse :: Maybe [Aeson.Value] -> Maybe Aeson.Value -> JsonWspResponse
mkGetDatumsByHashesResponse = \case
  Just datumsWithValues ->
    JsonWspResponse "GetDatumsByHashes" (object ["DatumsFound" .= value])
    where
      value = object ["value" .= datumsWithValues]
  Nothing ->
    JsonWspResponse "GetDatumsByHashes" (object ["DatumsNotFound" .= Null])

mkGetDatumsByHashesFault :: Text -> Maybe Aeson.Value -> JsonWspFault
mkGetDatumsByHashesFault =
  JsonWspFault "GetDatumsByHashes" "client"

mkGetBlockResponse :: BlockInfo -> Maybe Aeson.Value -> JsonWspResponse
mkGetBlockResponse block = JsonWspResponse "GetBlock" (object ["block" .= block])

mkGetBlockFault :: Maybe Aeson.Value -> JsonWspFault
mkGetBlockFault = JsonWspFault "GetBlock" "notFound" ""

mkStartFetchBlocksResponse :: Maybe Aeson.Value -> JsonWspResponse
mkStartFetchBlocksResponse =
  JsonWspResponse "StartFetchBlocks" (object ["StartedBlockFetcher" .= Bool True])

mkStartFetchBlocksFault :: Text -> Maybe Aeson.Value -> JsonWspFault
mkStartFetchBlocksFault =
  JsonWspFault "StartFetchBlocks" "client"

mkCancelFetchBlocksResponse :: Maybe Aeson.Value -> JsonWspResponse
mkCancelFetchBlocksResponse =
  JsonWspResponse "CancelFetchBlocks" (object ["StoppedBlockFetcher" .= Bool True])

mkCancelFetchBlocksFault :: Text -> Maybe Aeson.Value -> JsonWspFault
mkCancelFetchBlocksFault =
  JsonWspFault "CancelFetchBlocks" "client"

mkHealthcheckResponse :: Maybe Aeson.Value -> JsonWspResponse
mkHealthcheckResponse =
  JsonWspResponse "GetHealthcheck" (object [])
