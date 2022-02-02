{-# LANGUAGE DuplicateRecordFields #-}
module Api.WebSocket.Json where

import GHC.Generics (Generic)
import qualified Data.Aeson as Json
import Data.Text (Text)

import Data.Aeson

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
  deriving stock Generic

instance ToJSON JsonWspResponse where
  toJSON (JsonWspResponse method res) = object
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

instance ToJSON JsonWspFault where
  toJSON (JsonWspFault method code str) = object
    [ "type" .= ("jsonwsp/fault" :: Text)
    , "version" .= ("1.0" :: Text)
    , "servicename" .= ("ogmios-datum-cache" :: Text)
    , "methodname" .= method
    , "fault" .= object [ "code" .= code
                        , "string" .= str
                        ]
    ]

mkGetDatumByHashResponse :: Maybe Json.Value -> JsonWspResponse
mkGetDatumByHashResponse = \case
  Just datumValue ->
    JsonWspResponse "GetDatumByHash" (object [ "DatumFound" .= value ])
    where
      value = object [ "value" .= datumValue ]
  Nothing ->
    JsonWspResponse "GetDatumByHash" (object [ "DatumNotFound" .= Null ])

mkGetDatumByHashFault :: Text -> JsonWspFault
mkGetDatumByHashFault str =
  JsonWspFault "GetDatumByHash" "client" str

mkGetDatumsByHashesResponse :: Maybe [Json.Value] -> JsonWspResponse
mkGetDatumsByHashesResponse = \case
  Just datumsWithValues ->
    JsonWspResponse "GetDatumsByHashes" (object [ "DatumsFound" .= value ])
    where
      value = object [ "value" .= datumsWithValues ]
  Nothing ->
    JsonWspResponse "GetDatumsByHashes" (object [ "DatumsNotFound" .= Null ])

mkGetDatumsByHashesFault :: Text -> JsonWspFault
mkGetDatumsByHashesFault str =
  JsonWspFault "GetDatumsByHashes" "client" str

mkStartFetchBlocksResponse :: JsonWspResponse
mkStartFetchBlocksResponse =
  JsonWspResponse "StartFetchBlocks" (object [ "StartedBlockFetcher" .= Bool True ])

mkStartFetchBlocksFault :: Text -> JsonWspFault
mkStartFetchBlocksFault str =
  JsonWspFault "StartFetchBlocks" "client" str

mkCancelFetchBlocksResponse :: JsonWspResponse
mkCancelFetchBlocksResponse =
  JsonWspResponse "CancelFetchBlocks" (object [ "StoppedBlockFetcher" .= Bool True ])

mkCancelFetchBlocksFault :: Text -> JsonWspFault
mkCancelFetchBlocksFault str =
  JsonWspFault "CancelFetchBlocks" "client" str
