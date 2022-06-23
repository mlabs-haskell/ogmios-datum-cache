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
  CursorPoint (..),
  StartingBlock (..),
) where

import Data.Aeson (FromJSON, ToJSON, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (prependFailure, unexpected)
import Data.HashMap.Strict qualified as HashMap
import Data.Int (Int64)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Exts (toList)
import GHC.Generics (Generic)

data StartingBlock = StartingBlock BlockInfo | Origin
  deriving stock (Show, Eq)

instance ToJSON StartingBlock where
  toJSON Origin = "origin"
  toJSON (StartingBlock (BlockInfo slot hash)) =
    Aeson.object
      [ "blockSlot" .= slot
      , "blockId" .= hash
      ]

instance FromJSON StartingBlock where
  parseJSON (Aeson.String "origin") = pure Origin
  parseJSON (Aeson.Object v) =
    fmap StartingBlock $
      BlockInfo
        <$> v .: "blockSlot"
        <*> v .: "blockId"
  parseJSON invalid = prependFailure "parsing StartingBlock failed" $ unexpected invalid

data BlockInfo = BlockInfo
  { blockSlot :: Int64
  , blockId :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

type OgmiosMirror = Int

data CursorPoint
  = CursorPoint Integer Text
  | CursorOrigin
  deriving stock (Eq, Show)

instance ToJSON CursorPoint where
  toJSON CursorOrigin = "origin"
  toJSON (CursorPoint slot hash) =
    Aeson.object
      [ "slot" .= slot
      , "hash" .= hash
      ]

instance FromJSON CursorPoint where
  parseJSON (Aeson.String "origin") = pure CursorOrigin
  parseJSON (Aeson.Object v) =
    CursorPoint
      <$> v .: "slot"
      <*> v .: "hash"
  parseJSON invalid = prependFailure "parsing CursorPoint failed" $ unexpected invalid

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

mkFindIntersectRequest :: StartingBlock -> OgmiosFindIntersectRequest
mkFindIntersectRequest startingBlock =
  OgmiosRequest
    { _type = "jsonwsp/request"
    , _version = "1.0"
    , _servicename = "ogmios"
    , _methodname = "FindIntersect"
    , _args = points
    , _mirror = 0
    }
  where
    points = case startingBlock of
      StartingBlock (BlockInfo firstBlockSlot firstBlockId) ->
        CursorPoints [CursorPoint (fromIntegral firstBlockSlot) firstBlockId]
      Origin -> CursorPoints [CursorOrigin]

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
  = IntersectionFound CursorPoint ResultTip
  | IntersectionNotFound ResultTip
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
  = RollBackward CursorPoint ResultTip
  | RollForward Block ResultTip
  deriving stock (Eq, Show, Generic)

data Block
  = OtherBlock String
  | MkByronBlock ByronBlock
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
  , headerHash :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data ByronBlock = ByronBlock
  { hash :: Text
  , slot :: Int64
  }
  deriving stock (Eq, Show)

instance FromJSON ByronBlock where
  parseJSON = Aeson.withObject "ByronBlock" $ \v -> do
    hash <- v .: "hash"
    header <- v .: "header"
    slot <- header .: "slot"
    pure $ ByronBlock hash slot

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
                [("byron" :: Text, blockValue)] -> do
                  block <- Aeson.parseJSON @ByronBlock blockValue
                  pure $ RollForward (MkByronBlock block) tip
                [("alonzo" :: Text, blockValue)] -> do
                  block <- Aeson.parseJSON @AlonzoBlock blockValue
                  pure $ RollForward (MkAlonzoBlock block) tip
                [(_, _blockObj)] ->
                  pure $ RollForward (OtherBlock $ show blockObj) tip
                _ -> fail "Unexpected block value"
          )
          rollObj
      _ -> fail "Unexpected object key"
