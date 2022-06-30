{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

{- HLINT ignore "Use ==" -}
module Block.Types (
  mkFindIntersectRequest,
  mkRequestNextRequest,
  OgmiosFindIntersectResponse,
  OgmiosRequestNextResponse,
  FindIntersectResult (..),
  RequestNextResult (..),
  Block (..),
  DatumBlock (..),
  DatumBlockHeader (..),
  DatumTransaction (..),
  DatumTxOut (..),
  OgmiosResponse (..),
  BlockInfo (..),
  CursorPoint (..),
  datums,
  noDatum2datumBlock,
  StartingBlock (..),
  RawTransaction (..),
) where

import Data.Aeson (FromJSON, ToJSON, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (prependFailure, unexpected)
import Data.HashMap.Strict qualified as HashMap
import Data.Int (Int64)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.String.ToString (toString)
import Data.Text (Text)
import Data.Text qualified as Text
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
  = UnsupportedBlock
      Text -- block type
      Text -- raw
  | -- | Block with datum (Alonzo, Babbage)
    MkDatumBlock
      Text -- block type
      DatumBlock
  | MkNoDatumBlock
      Text -- block type
      NoDatumBlock
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
                [(type_ :: Text, blockValue)]
                  | type_ `elem` ["byron"] -> do
                    block <- Aeson.parseJSON @NoDatumBlock blockValue
                    pure $ RollForward (MkNoDatumBlock type_ block) tip
                  | type_ `elem` ["alonzo", "babbage"] -> do
                    block <- Aeson.parseJSON @DatumBlock blockValue
                    pure $ RollForward (MkDatumBlock type_ block) tip
                  | otherwise -> do
                    let raw = Text.pack $ toString $ Aeson.encode blockValue
                    pure $ RollForward (UnsupportedBlock type_ raw) tip
                _ -> fail "Unexpected block value"
          )
          rollObj
      _ -> fail "Unexpected object key"

data RawTransaction = RawTransaction
  { txId :: Text
  , rawTx :: Aeson.Value
  }
  deriving stock (Eq, Show)

instance FromJSON RawTransaction where
  parseJSON = Aeson.withObject "RawTransaction" $ \v -> do
    RawTransaction
      <$> v .: "id"
      <*> pure (Aeson.Object v)

data DatumBlock = DatumBlock
  { body :: [DatumTransaction]
  , rawTransactions :: [RawTransaction]
  , header :: DatumBlockHeader
  , headerHash :: Text
  }
  deriving stock (Eq, Show)

instance FromJSON DatumBlock where
  parseJSON = Aeson.withObject "DatumBlock" $ \v ->
    DatumBlock
      <$> v .: "body"
      <*> v .: "body"
      <*> v .: "header"
      <*> v .: "headerHash"

data DatumBlockHeader = DatumBlockHeader
  { slot :: Int64
  , blockHash :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data DatumTransaction = DatumTransaction
  { dtDatums :: Map Text Text
  , dtOutputs :: [DatumTxOut]
  }
  deriving stock (Eq, Show, Generic)

datums :: DatumTransaction -> Map Text Text
datums DatumTransaction {dtDatums, dtOutputs} =
  dtDatums <> Map.fromList (mapMaybe fromTxOut dtOutputs)
  where
    fromTxOut
      DatumTxOut
        { datumHash = Just datumHash
        , datum = Just datum
        } = Just (datumHash, datum)
    fromTxOut _txOut =
      Nothing

instance FromJSON DatumTransaction where
  parseJSON = withObject "DatumTransaction" $ \o -> do
    witness <- o .: "witness"
    dtDatums <- witness .: "datums"
    body <- o .: "body"
    dtOutputs <- body .: "outputs"
    pure $ DatumTransaction {..}

data DatumTxOut = DatumTxOut
  { address :: Text
  , datumHash :: Maybe Text
  , datum :: Maybe Text
  -- , script :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON DatumTxOut where
  parseJSON = withObject "BabbageTxOut" $ \o -> do
    address <- o .: "address"
    datumHash <- o .:? "datumHash"
    datum <- o .:? "datum"
    pure $ DatumTxOut {..}

data NoDatumBlock = NoDatumBlock
  { hash :: Text
  , slot :: Int64
  }
  deriving stock (Eq, Show)

instance FromJSON NoDatumBlock where
  parseJSON = Aeson.withObject "NoDatumBlock" $ \v -> do
    hash <- v .: "hash"
    header <- v .: "header"
    slot <- header .: "slot"
    pure $ NoDatumBlock hash slot

noDatum2datumBlock :: NoDatumBlock -> DatumBlock
noDatum2datumBlock NoDatumBlock {slot, hash} = DatumBlock [] [] (DatumBlockHeader slot hash) hash
