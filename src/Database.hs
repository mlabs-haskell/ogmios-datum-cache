module Database (
  initTables,
  Datum (..),
  DatabaseError (..),
  getDatumByHash,
  getDatumsByHashes,
  saveDatums,
  initLastBlock,
  updateLastBlock,
  getLastBlock,
  saveTxs,
  getTxByHash,
) where

import Codec.Serialise (DeserialiseFailure, deserialiseOrFail)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logErrorNS, logInfoNS)
import Control.Monad.Reader.Has (Has, MonadReader, ask)
import Control.Monad.Trans.Except (except, runExceptT, throwE)
import Data.Aeson (ToJSON (toJSON), (.=))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Functor.Contravariant ((>$<))
import Data.List (foldl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import Hasql.Connection (Connection)
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session (Session)
import Hasql.Session qualified as Session
import Hasql.Statement (Statement (Statement))

import Block.Types (
  BlockInfo (BlockInfo),
  SomeRawTransaction (AlonzoRawTransaction, BabbageRawTransaction),
  getRawTx,
  getRawTxId,
 )
import Block.Types.Alonzo qualified as Alonzo
import Block.Types.Babbage qualified as Babbage
import DataHash (DataHash (DataHash, unDataHash))
import PlutusData qualified

data Datum = Datum
  { hash :: DataHash
  , value :: ByteString
  }
  deriving stock (Eq, Show)

getDatumSession :: DataHash -> Session Datum
getDatumSession datumHash =
  Session.statement datumHash getDatumStatement

getDatumStatement :: Statement DataHash Datum
getDatumStatement = Statement sql enc dec True
  where
    sql =
      "SELECT hash, value FROM datums WHERE hash = $1"
    enc =
      Encoders.param (Encoders.nonNullable (unDataHash >$< Encoders.text))
    dec =
      Decoders.singleRow $
        Datum
          <$> Decoders.column
            (Decoders.nonNullable (DataHash <$> Decoders.text))
          <*> Decoders.column (Decoders.nonNullable Decoders.bytea)

getDatumsSession :: [DataHash] -> Session (Vector Datum)
getDatumsSession datumsHashes =
  Session.statement datumsHashes getDatumsStatement

getDatumsStatement :: Statement [DataHash] (Vector Datum)
getDatumsStatement = Statement sql enc dec True
  where
    sql =
      "SELECT hash, value FROM datums WHERE hash = ANY ($1)"
    enc =
      Encoders.param $
        Encoders.nonNullable $
          Encoders.array $
            Encoders.dimension foldl' $
              Encoders.element (Encoders.nonNullable (unDataHash >$< Encoders.text))
    dec =
      Decoders.rowVector $
        Datum
          <$> Decoders.column
            (Decoders.nonNullable (DataHash <$> Decoders.text))
          <*> Decoders.column (Decoders.nonNullable Decoders.bytea)

insertDatumsSession :: [DataHash] -> [ByteString] -> Session ()
insertDatumsSession datumHashes datumValues = do
  Session.statement (datumHashes, datumValues) insertDatumsStatement

insertDatumsStatement :: Statement ([DataHash], [ByteString]) ()
insertDatumsStatement = Statement sql enc dec True
  where
    sql =
      "INSERT INTO datums (hash, value) (SELECT h::text, v::bytea FROM unnest($1, $2) AS x(h, v)) ON CONFLICT(hash) DO UPDATE SET value=EXCLUDED.value"

    encArray elemEncoder =
      Encoders.param $
        Encoders.nonNullable $
          Encoders.array $
            Encoders.dimension foldl' $
              Encoders.element $
                Encoders.nonNullable elemEncoder
    enc =
      (fst >$< encArray (unDataHash >$< Encoders.text))
        <> (snd >$< encArray Encoders.bytea)

    dec = Decoders.noResult

insertRawTransactionsStatement :: Statement [SomeRawTransaction] ()
insertRawTransactionsStatement = Statement sql enc dec True
  where
    sql =
      "INSERT INTO transactions (txId, txType, rawTx) (SELECT h::text, t::text, v::bytea FROM unnest($1, $2, $3::bytea[]) AS x(h, t, v)) ON CONFLICT(txId) DO UPDATE SET txType=EXCLUDED.txType, rawTx=EXCLUDED.rawTx"

    encArray elemEncoder =
      Encoders.param $
        Encoders.nonNullable $
          Encoders.array $
            Encoders.dimension foldl' $
              Encoders.element $
                Encoders.nonNullable elemEncoder
    enc :: Encoders.Params [SomeRawTransaction] =
      (fmap encId >$< encArray Encoders.text)
        <> (fmap encType >$< encArray Encoders.text)
        <> (fmap getRawTx >$< encArray Encoders.bytea)
    encType (AlonzoRawTransaction _) = "alonzo"
    encType (BabbageRawTransaction _) = "babbage"
    encId (AlonzoRawTransaction tx) = tx.txId
    encId (BabbageRawTransaction tx) = tx.txId
    dec = Decoders.noResult

insertRawTransactionsSession :: [SomeRawTransaction] -> Session ()
insertRawTransactionsSession txs = Session.statement txs insertRawTransactionsStatement

getRawTxStatement :: Statement Text SomeRawTransaction
getRawTxStatement = Statement sql enc dec True
  where
    sql = "SELECT txId, txType, rawTx FROM transactions WHERE txId = $1"
    enc = Encoders.param (Encoders.nonNullable Encoders.text)
    dec =
      Decoders.singleRow $ do
        txId <- Decoders.column $ Decoders.nonNullable Decoders.text
        ty <- Decoders.column $ Decoders.nonNullable Decoders.text
        raw <- Decoders.column (Decoders.nonNullable Decoders.bytea)
        case ty of
          "alonzo" ->
            pure $ AlonzoRawTransaction $ Alonzo.RawTransaction txId raw
          "babbage" ->
            pure $ BabbageRawTransaction $ Babbage.RawTransaction txId raw
          _ -> error "unreachable: getRawTxStatement"

getRawTxSession :: Text -> Session SomeRawTransaction
getRawTxSession txId = Session.statement txId getRawTxStatement

initTables :: (MonadIO m, MonadReader r m, Has Connection r) => m ()
initTables = do
  let sql = do
        Session.sql "CREATE TABLE IF NOT EXISTS datums (hash text, value bytea);"
        Session.sql "CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS datums_hash_index ON datums (hash);"
        Session.sql
          "CREATE TABLE IF NOT EXISTS last_block \
          \ (onerow_id bool PRIMARY KEY DEFAULT TRUE, slot integer, hash text, CONSTRAINT onerow CHECK (onerow_id))"
        Session.sql "CREATE TABLE IF NOT EXISTS transactions (txId text PRIMARY KEY, txType text, rawTx bytea);"
        Session.sql
          "DELETE FROM transactions a USING ( \
          \  SELECT MIN(ctid) as ctid, txId FROM transactions \
          \    GROUP BY txId HAVING COUNT(*) > 1 ) b \
          \  WHERE a.txId = b.txId AND a.ctid != b.ctid;"
        Session.sql
          "ALTER TABLE transactions \
          \  ADD CONSTRAINT transactions_pkey PRIMARY KEY (txId);"
        Session.sql "CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS transactions ON datums (txId);"
  conn <- ask
  liftIO $ void $ Session.run sql conn

initLastBlock ::
  ( MonadIO m
  , MonadLogger m
  , MonadReader r m
  , Has Connection r
  ) =>
  BlockInfo ->
  m ()
initLastBlock (BlockInfo slot hash) = do
  let sql = "INSERT INTO last_block (slot, hash) VALUES ($1, $2) ON CONFLICT DO NOTHING"
      enc =
        mconcat
          [ fst >$< Encoders.param (Encoders.nonNullable Encoders.int8)
          , snd >$< Encoders.param (Encoders.nonNullable Encoders.text)
          ]
      dec = Decoders.noResult
      stmt = Session.statement (slot, hash) $ Statement sql enc dec True
  dbConnection <- ask
  res <- liftIO $ Session.run stmt dbConnection
  case res of
    Right _ -> pure ()
    Left err -> do
      logErrorNS "initLastBlock" $ Text.pack $ show err
      pure ()

updateLastBlock ::
  ( MonadIO m
  , MonadLogger m
  ) =>
  Connection ->
  BlockInfo ->
  m ()
updateLastBlock dbConnection (BlockInfo slot hash) = do
  let sql = "UPDATE last_block SET slot = $1, hash = $2"
      enc =
        mconcat
          [ fst >$< Encoders.param (Encoders.nonNullable Encoders.int8)
          , snd >$< Encoders.param (Encoders.nonNullable Encoders.text)
          ]
      dec = Decoders.noResult
      stmt = Session.statement (slot, hash) $ Statement sql enc dec True
  res <- liftIO $ Session.run stmt dbConnection
  case res of
    Right _ -> pure ()
    Left err -> do
      logErrorNS "updateLastBlock" $ Text.pack $ show err
      pure ()

getLastBlock ::
  ( MonadIO m
  , MonadLogger m
  ) =>
  Connection ->
  m (Maybe BlockInfo)
getLastBlock dbConnection = do
  let sql = "SELECT slot, hash FROM last_block LIMIT 1"
      enc = Encoders.noParams
      dec =
        Decoders.singleRow $
          BlockInfo
            <$> Decoders.column (Decoders.nonNullable Decoders.int8)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
      stmt = Session.statement () $ Statement sql enc dec True
  res <- liftIO $ Session.run stmt dbConnection
  case res of
    Right x -> pure . pure $ x
    Left err -> do
      logErrorNS "getLastBlock" $ Text.pack $ show err
      pure Nothing

data DatabaseError
  = DatabaseErrorDecodeError ByteString DeserialiseFailure
  | DatabaseErrorNotFound
  deriving stock (Generic, Show)

instance ToJSON DatabaseError where
  toJSON (DatabaseErrorDecodeError value err) =
    Aeson.object
      [ "DecodingError : " .= Text.pack (show value)
      , "error" .= Text.pack (show err)
      ]
  toJSON DatabaseErrorNotFound = Aeson.object [("error", "NotFoundError")]

toPlutusData :: Datum -> Either DatabaseError PlutusData.Data
toPlutusData datum =
  let res = deserialiseOrFail @PlutusData.Data (BSL.fromStrict $ value datum)
   in case res of
        Left err -> Left $ DatabaseErrorDecodeError (value datum) err
        Right x -> pure x

toPlutusDataMany ::
  Vector Datum ->
  Map DataHash (Either DatabaseError PlutusData.Data)
toPlutusDataMany = Map.fromList . Vector.toList . (deserialiseDatum <$>)
  where
    deserialiseDatum ::
      Datum ->
      (DataHash, Either DatabaseError PlutusData.Data)
    deserialiseDatum d =
      ( hash d
      , first
          (DatabaseErrorDecodeError (value d))
          $ (deserialiseOrFail @PlutusData.Data . BSL.fromStrict . value) d
      )

getDatumByHash ::
  (MonadIO m, MonadReader r m, Has Connection r) =>
  DataHash ->
  m (Either DatabaseError PlutusData.Data)
getDatumByHash hash = runExceptT $ do
  conn <- ask
  res' <- liftIO (Session.run (getDatumSession hash) conn)
  case res' of
    Left _ -> throwE DatabaseErrorNotFound
    Right datum -> except $ toPlutusData datum

getDatumsByHashes ::
  (MonadIO m, MonadReader r m, Has Connection r) =>
  [DataHash] ->
  m (Either DatabaseError (Map DataHash (Either DatabaseError PlutusData.Data)))
getDatumsByHashes [] = (pure . pure) mempty
getDatumsByHashes hashes = runExceptT $ do
  conn <- ask
  res' <- liftIO (Session.run (getDatumsSession hashes) conn)
  case res' of
    Left _ -> throwE DatabaseErrorNotFound
    Right datums ->
      let deserializedDatums = toPlutusDataMany datums
          notFoundDatums =
            [ (hash, Left DatabaseErrorNotFound)
            | hash <- hashes
            , hash `Map.notMember` deserializedDatums
            ]
       in (except . pure) $ deserializedDatums `Map.union` (Map.fromList notFoundDatums)

saveDatums ::
  ( MonadIO m
  , MonadLogger m
  ) =>
  Connection ->
  [(DataHash, ByteString)] ->
  m ()
saveDatums dbConnection datums = do
  let (datumHashes, datumValues) = unzip datums
  logInfoNS "saveDatums" $
    "Inserting datums: " <> Text.intercalate ", " (unDataHash <$> datumHashes)
  res <-
    liftIO $
      Session.run (insertDatumsSession datumHashes datumValues) dbConnection
  case res of
    Right _ -> pure ()
    Left err -> do
      logErrorNS "saveDatums" $
        "Error inserting datums: " <> Text.pack (show err)
      pure ()

saveTxs ::
  (MonadIO m, MonadLogger m) =>
  Connection ->
  [SomeRawTransaction] ->
  m ()
saveTxs dbConnection txs = do
  logInfoNS "saveTxs" $
    "Inserting transactions: " <> Text.intercalate ", " (fmap getRawTxId txs)
  res <-
    liftIO $
      Session.run (insertRawTransactionsSession txs) dbConnection
  case res of
    Right _ -> pure ()
    Left err -> do
      logErrorNS "saveTxs" $
        "Error inserting txs: " <> Text.pack (show err)
      pure ()

getTxByHash ::
  (MonadIO m, MonadReader r m, Has Connection r) =>
  Text ->
  m (Either DatabaseError SomeRawTransaction)
getTxByHash txId = do
  conn <- ask
  res' <- liftIO (Session.run (getRawTxSession txId) conn)
  case res' of
    Left _ -> pure $ Left DatabaseErrorNotFound
    Right tx -> pure $ Right tx
