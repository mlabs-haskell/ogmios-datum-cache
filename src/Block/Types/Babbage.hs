module Block.Types.Babbage (
  TxOut (..),
  Transaction (..),
  RawTransaction (..),
  BlockHeader (..),
  Block (..),
  datumsInTxOut,
  datumsInTransaction,
) where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import Data.Int (Int64)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)

import DataHash (DataHash (DataHash))

data TxOut = TxOut
  { address :: Text
  , datumHash :: Maybe DataHash
  , datum :: Maybe Text
  }
  deriving stock (Eq, Show)

instance FromJSON TxOut where
  parseJSON = withObject "TxOut" $ \o -> do
    TxOut
      <$> o .: "address"
      <*> ((DataHash <$>) <$> o .:? "datumHash")
      <*> o .:? "datum"

data Transaction = Transaction
  { datums :: Map DataHash Text
  , outputs :: [TxOut]
  }
  deriving stock (Eq, Show)

instance FromJSON Transaction where
  parseJSON = withObject "Transaction" $ \o -> do
    witness <- o .: "witness"
    datums <- witness .: "datums"
    body <- o .: "body"
    outputs <- body .: "outputs"
    pure $ Transaction datums outputs

data RawTransaction = RawTransaction
  { txId :: Text
  , rawTx :: Text
  }
  deriving stock (Eq, Show)

instance FromJSON RawTransaction where
  parseJSON = withObject "RawTransaction" $ \v -> do
    RawTransaction
      <$> v .: "id"
      <*> v .: "raw"

data BlockHeader = BlockHeader
  { slot :: Int64
  , blockHash :: Text
  }
  deriving stock (Eq, Show)

instance FromJSON BlockHeader where
  parseJSON = withObject "BlockHeader" $ \o ->
    BlockHeader
      <$> o .: "slot"
      <*> o .: "blockHash"

data Block = Block
  { body :: [Transaction]
  , rawTransactions :: [RawTransaction]
  , header :: BlockHeader
  , headerHash :: Text
  }
  deriving stock (Eq, Show)

instance FromJSON Block where
  parseJSON = withObject "Block" $ \v ->
    Block
      <$> v .: "body"
      <*> v .: "body"
      <*> v .: "header"
      <*> v .: "headerHash"

datumsInTxOut :: TxOut -> Map DataHash Text
datumsInTxOut txOut
  | Just dh <- txOut.datumHash
    , Just d <- txOut.datum =
    Map.singleton dh d
  | otherwise = mempty

datumsInTransaction :: Transaction -> Map DataHash Text
datumsInTransaction tx = tx.datums <> foldMap datumsInTxOut tx.outputs
