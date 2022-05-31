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
  TxOut (..),
  BlockInfo (..),
  blockId,
  blockSlot,
) where

import Data.Aeson (FromJSON, ToJSON, Value (Object, String), withObject, withText, (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (toStrict)
import Data.HashMap.Strict qualified as HashMap
import Data.Int (Int64)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import GHC.Exts (toList)
import GHC.Generics (Generic)

data BlockInfo = BlockInfo Int64 Text | BlockOrigin
  deriving stock (Generic, Show, Eq)

instance ToJSON BlockInfo where
  toJSON BlockOrigin =
    Aeson.String "origin"
  toJSON (BlockInfo slot' id') =
    Aeson.object
      [ "blockSlot" Aeson..= slot'
      , "blockId" Aeson..= id'
      ]

instance FromJSON BlockInfo where
  parseJSON val =
    ( case val of
        (Object _) -> withObject "BlockInfo" $ \object -> BlockInfo <$> (object .: "blockSlot") <*> (object .: "blockId")
        (String "origin") -> withText "BlockInfo" $ const $ return BlockOrigin
        _ -> error "Somehow origin was read in from Ogmios"
    )
      val

blockSlot :: BlockInfo -> Int64
blockSlot (BlockInfo slot' _) = slot'
blockSlot BlockOrigin = 0

blockId :: BlockInfo -> Text
blockId (BlockInfo _ id') = id'
blockId BlockOrigin = "0"

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

type OgmiosFindIntersectRequest = OgmiosRequest Text OgmiosMirror

type OgmiosRequestNextRequest = OgmiosRequest (Map Text Text) OgmiosMirror

mkFindIntersectRequest :: BlockInfo -> OgmiosFindIntersectRequest
mkFindIntersectRequest blockInfo =
  OgmiosRequest
    { _type = "jsonwsp/request"
    , _version = "1.0"
    , _servicename = "ogmios"
    , _methodname = "FindIntersect"
    , _args = payload
    , _mirror = 0
    }
  where
    points :: Int64 -> Text -> CursorPoints
    points s i = CursorPoints [CursorPoint (fromIntegral s) i]
    payload :: Text
    payload = decodeUtf8 $
      toStrict $ case blockInfo of
        BlockInfo slot hash -> Aeson.encode $ points slot hash
        BlockOrigin -> Aeson.encode [BlockOrigin]

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
  = OtherBlock
  | MkAlonzoBlock AlonzoBlock
  deriving stock (Eq, Show, Generic)

data TxOut = TxOut
  { address :: Text
  , datumHash :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON TxOut where
  parseJSON = withObject "TxOut" $ \o -> do
    address <- o .: "address"
    datumHash <- o .:? "datum"
    pure $ TxOut {..}

data AlonzoTransaction = AlonzoTransaction
  { datums :: Map Text Text
  , outputs :: [TxOut]
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON AlonzoTransaction where
  parseJSON = withObject "AlonzoTransaction" $ \o -> do
    witness <- o .: "witness"
    datums <- witness .: "datums"
    body <- o .: "body"
    outputs <- body .: "outputs"
    pure $ AlonzoTransaction datums outputs

data AlonzoBlockHeader = AlonzoBlockHeader
  { slot :: Int64
  , blockHash :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data AlonzoBlock = AlonzoBlock
  { body :: [AlonzoTransaction]
  , header :: AlonzoBlockHeader
  , headerHash :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

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
                  pure $ RollForward OtherBlock tip
                _ -> fail "Unexpected block value"
          )
          rollObj
      _ -> fail "Unexpected object key"
