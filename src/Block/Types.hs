{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Block.Types (
  mkFindIntersectRequest,
  mkRequestNextRequest,
  OgmiosFindIntersectResponse,
  OgmiosRequestNextResponse,
  FindIntersectResult (..),
  RequestNextResult (..),
  Block (..),
  Transaction (..),
  TxOut (..),
  AlonzoBlock (..),
  AlonzoBlockHeader (..),
  AlonzoTransaction (..),
  AlonzoTxOut (..),
  BabbageBlock (..),
  BabbageBlockHeader (..),
  BabbageTransaction (..),
  BabbageTxOut (..),
  OgmiosResponse (..),
  BlockInfo (..),
) where

import Data.Aeson (FromJSON, ToJSON, withObject, (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict qualified as HashMap
import Data.Int (Int64)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
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
  | MkBabbageBlock BabbageBlock
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
                [("babbage" :: Text, blockValue)] -> do
                  block <- Aeson.parseJSON @BabbageBlock blockValue
                  pure $ RollForward (MkBabbageBlock block) tip
                [(_, _blockObj)] ->
                  pure $ RollForward (OtherBlock $ Aeson.encode blockObj) tip
                _ -> fail "Unexpected block value"
          )
          rollObj
      _ -> fail "Unexpected object key"

class TxOut out => Transaction tx out | tx -> out where
  datums :: tx -> Map Text Text
  outputs :: tx -> [out]

class TxOut out where
  address :: out -> Text
  datumHash :: out -> Maybe Text

-- * Alonzo

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

instance Transaction AlonzoTransaction AlonzoTxOut where
  datums = abDatums
  outputs = abOutputs

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

instance TxOut AlonzoTxOut where
  address = abAddress
  datumHash = abDatumHash

instance FromJSON AlonzoTxOut where
  parseJSON = withObject "AlonzoTxOut" $ \o -> do
    abAddress <- o .: "address"
    abDatumHash <- o .:? "datum" -- it is right for alonzo version!
    pure $ AlonzoTxOut {..}

-- * Babbage

data BabbageBlock = BabbageBlock
  { body :: [BabbageTransaction]
  , header :: BabbageBlockHeader
  , headerHash :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data BabbageBlockHeader = BabbageBlockHeader
  { slot :: Int64
  , blockHash :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data BabbageTransaction = BabbageTransaction
  { bbDatums :: Map Text Text
  , bbOutputs :: [BabbageTxOut]
  }
  deriving stock (Eq, Show, Generic)

instance Transaction BabbageTransaction BabbageTxOut where
  datums BabbageTransaction {bbDatums, bbOutputs} =
    bbDatums <> Map.fromList (catMaybes $ map fromTxOut bbOutputs)
    where
      fromTxOut
        BabbageTxOut
          { bbDatumHash = Just bbDatumHash
          , bbDatum = Just bbDatum
          } = Just (bbDatumHash, bbDatum)
      fromTxOut _txOut =
        Nothing
  outputs = bbOutputs

instance FromJSON BabbageTransaction where
  parseJSON = withObject "BabbageTransaction" $ \o -> do
    witness <- o .: "witness"
    bbDatums <- witness .: "datums"
    body <- o .: "body"
    bbOutputs <- body .: "outputs"
    pure $ BabbageTransaction {..}

data BabbageTxOut = BabbageTxOut
  { bbAddress :: Text
  , bbDatumHash :: Maybe Text
  , bbDatum :: Maybe Text
  -- , bbScript :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance TxOut BabbageTxOut where
  address = bbAddress
  datumHash = bbDatumHash

instance FromJSON BabbageTxOut where
  parseJSON = withObject "BabbageTxOut" $ \o -> do
    bbAddress <- o .: "address"
    bbDatumHash <- o .:? "datumHash"
    bbDatum <- o .:? "datum"
    pure $ BabbageTxOut {..}
