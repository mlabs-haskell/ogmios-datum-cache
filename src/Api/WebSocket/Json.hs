module Api.WebSocket.Json (
  JsonWspResponse,
  JsonWspFault (JsonWspFault),
  mkGetDatumByHashResponse,
  mkGetDatumByHashFault,
  mkGetTxByHashResponse,
  mkGetTxByHashResponseFault,
  mkGetDatumsByHashesResponse,
  mkGetDatumsByHashesFault,
  mkGetBlockResponse,
  mkGetBlockFault,
  mkHealthcheckResponse,
  mkSetStartingBlockResponse,
  mkSetStartingBlockFault,
  mkSetDatumFilterResponse,
) where

import Data.Aeson (
  KeyValue ((.=)),
  ToJSON (toJSON),
  Value (Null),
  object,
 )
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)

import Block.Types (BlockInfo, CursorPoint, SomeRawTransaction, getRawTx)
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

mkGetDatumsByHashesResponse :: Aeson.Value -> Maybe Aeson.Value -> JsonWspResponse
mkGetDatumsByHashesResponse datumsWithValues =
  JsonWspResponse "GetDatumsByHashes" (object ["value" .= datumsWithValues])

mkGetDatumsByHashesFault :: Text -> Maybe Aeson.Value -> JsonWspFault
mkGetDatumsByHashesFault =
  JsonWspFault "GetDatumsByHashes" "client"

mkGetTxByHashResponse :: Maybe SomeRawTransaction -> Maybe Aeson.Value -> JsonWspResponse
mkGetTxByHashResponse = \case
  Just tx ->
    JsonWspResponse "GetTxByHash" (object ["TxFound" .= value])
    where
      value = object ["value" .= (decodeUtf8 . getRawTx) tx]
  Nothing ->
    JsonWspResponse "GetTxByHash" (object ["TxNotFound" .= Null])

mkGetTxByHashResponseFault :: Text -> Maybe Aeson.Value -> JsonWspFault
mkGetTxByHashResponseFault =
  JsonWspFault "GetTxByHash" "client"

mkGetBlockResponse :: BlockInfo -> Maybe Aeson.Value -> JsonWspResponse
mkGetBlockResponse block = JsonWspResponse "GetBlock" (object ["block" .= block])

mkGetBlockFault :: Maybe Aeson.Value -> JsonWspFault
mkGetBlockFault = JsonWspFault "GetBlock" "notFound" ""

mkHealthcheckResponse :: Maybe Aeson.Value -> JsonWspResponse
mkHealthcheckResponse =
  JsonWspResponse "GetHealthcheck" (object [])

mkSetStartingBlockResponse :: CursorPoint -> Maybe Aeson.Value -> JsonWspResponse
mkSetStartingBlockResponse point =
  JsonWspResponse "SetStartingBlock" (object ["Intersecton" .= point])

mkSetStartingBlockFault :: Maybe Aeson.Value -> JsonWspFault
mkSetStartingBlockFault = JsonWspFault "SetStartingBlock" "notFound" ""

mkSetDatumFilterResponse :: Maybe Aeson.Value -> JsonWspResponse
mkSetDatumFilterResponse =
  JsonWspResponse "SetDatumFilter" (object [])
