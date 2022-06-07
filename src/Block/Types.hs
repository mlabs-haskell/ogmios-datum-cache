{-# LANGUAGE DuplicateRecordFields #-}

module Block.Types (
  mkFindIntersectRequest,
  mkRequestNextRequest,
  OgmiosFindIntersectResponse,
  OgmiosRequestNextResponse,
  FindIntersectResult (..),
  RequestNextResult (..),
  Block (..),
  AlonzoBlock (..),
  AlonzoBlockHeader (..),
  AlonzoTransaction (..),
  OgmiosResponse (..),
  AlonzoTxOut (..),
  BlockInfo (..),
) where

import Data.Aeson (FromJSON, ToJSON, withObject, (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict qualified as HashMap
import Data.Int (Int64)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Exts (toList)
import GHC.Generics (Generic)

data BlockInfo = BlockInfo
  { blockSlot :: Int64
  , blockId :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

type OgmiosMirror = Int

data CursorPoint = CursorPoint
  { slot :: Integer
  , hash :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype CursorPoints = CursorPoints
  { points :: [CursorPoint]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data OgmiosRequest args mirror = OgmiosRequest
  { _type :: Text
  , _version :: Text
  , _servicename :: Text
  , _methodname :: Text
  , _args :: args
  , _mirror :: mirror
  }
  deriving stock (Eq, Show, Generic)

instance (ToJSON args, ToJSON mirror) => ToJSON (OgmiosRequest args mirror) where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = drop 1}

type OgmiosFindIntersectRequest = OgmiosRequest CursorPoints OgmiosMirror

type OgmiosRequestNextRequest = OgmiosRequest (Map Text Text) OgmiosMirror

mkFindIntersectRequest :: BlockInfo -> OgmiosFindIntersectRequest
mkFindIntersectRequest (BlockInfo firstBlockSlot firstBlockId) =
  OgmiosRequest
    { _type = "jsonwsp/request"
    , _version = "1.0"
    , _servicename = "ogmios"
    , _methodname = "FindIntersect"
    , _args = points
    , _mirror = 0
    }
  where
    points =
      CursorPoints [CursorPoint (fromIntegral firstBlockSlot) firstBlockId]

mkRequestNextRequest :: Int -> OgmiosRequestNextRequest
mkRequestNextRequest n =
  OgmiosRequest
    { _type = "jsonwsp/request"
    , _version = "1.0"
    , _servicename = "ogmios"
    , _methodname = "RequestNext"
    , _args = Map.empty
    , _mirror = n
    }

data OgmiosResponse result reflection = OgmiosResponse
  { _type :: Text
  , _version :: Text
  , _servicename :: Text
  , _methodname :: Text
  , _result :: result
  , _reflection :: reflection
  }
  deriving stock (Eq, Show, Generic)

instance
  (FromJSON result, FromJSON reflection) =>
  FromJSON (OgmiosResponse result reflection)
  where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = drop 1}

type OgmiosFindIntersectResponse =
  OgmiosResponse FindIntersectResult OgmiosMirror

data ResultTip = ResultTip
  { slot :: Integer
  , hash :: Text
  , blockNo :: Integer
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data FindIntersectResult
  = IntersectionFound
      { point :: CursorPoint
      , tip :: ResultTip
      }
  | IntersectionNotFound {tip :: ResultTip}
  deriving stock (Eq, Show, Generic)

instance FromJSON FindIntersectResult where
  parseJSON = withObject "FindIntersectResult" $ \o -> do
    case toList o of
      [("IntersectionFound", intersectionObj)] ->
        withObject
          "IntersectionFound"
          ( \obj -> do
              point <- obj .: "point"
              tip <- obj .: "tip"
              pure $ IntersectionFound point tip
          )
          intersectionObj
      [("IntersectionNotFound", intersectionObj)] ->
        withObject
          "IntersectionNotFound"
          ( \obj -> do
              tip <- obj .: "tip"
              pure $ IntersectionNotFound tip
          )
          intersectionObj
      _ -> fail "Unexpected object key"

type OgmiosRequestNextResponse = OgmiosResponse RequestNextResult OgmiosMirror

data RequestNextResult
  = RollBackward
      { point :: CursorPoint
      , tip :: ResultTip
      }
  | RollForward
      { block :: Block
      , tip :: ResultTip
      }
  deriving stock (Eq, Show, Generic)

data Block
  = OtherBlock ByteString
  | MkAlonzoBlock AlonzoBlock
  deriving stock (Eq, Show, Generic)

instance FromJSON RequestNextResult where
  parseJSON = withObject "RequestNextResult" $ \o -> do
    case toList o of
      [("RollBackward", rollObj)] ->
        withObject
          "RollBackward"
          ( \obj -> do
              point <- obj .: "point"
              tip <- obj .: "tip"
              pure $ RollBackward point tip
          )
          rollObj
      [("RollForward", rollObj)] ->
        withObject
          "RollForward"
          ( \obj -> do
              tip <- obj .: "tip"
              blockObj <- obj .: "block"
              case HashMap.toList blockObj of
                [("alonzo" :: Text, blockValue)] -> do
                  block <- Aeson.parseJSON @AlonzoBlock blockValue
                  pure $ RollForward (MkAlonzoBlock block) tip
                [(_, _blockObj)] ->
                  pure $ RollForward (OtherBlock $ Aeson.encode blockObj) tip
                _ -> fail "Unexpected block value"
          )
          rollObj
      _ -> fail "Unexpected object key"

data AlonzoBlock = AlonzoBlock
  { body :: [AlonzoTransaction]
  , header :: AlonzoBlockHeader
  , headerHash :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data AlonzoBlockHeader = AlonzoBlockHeader
  { slot :: Int64
  , blockHash :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data AlonzoTransaction = AlonzoTransaction
  { abDatums :: Map Text Text
  , abOutputs :: [AlonzoTxOut]
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON AlonzoTransaction where
  parseJSON = withObject "AlonzoTransaction" $ \o -> do
    witness <- o .: "witness"
    abDatums <- witness .: "datums"
    body <- o .: "body"
    abOutputs <- body .: "outputs"
    pure $ AlonzoTransaction {..}

data AlonzoTxOut = AlonzoTxOut
  { abAddress :: Text
  , abDatumHash :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON AlonzoTxOut where
  parseJSON = withObject "AlonzoTxOut" $ \o -> do
    abAddress <- o .: "address"
    abDatumHash <- o .:? "datum" -- it is right for alonzo version!
    pure $ AlonzoTxOut {..}
