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
) where

import Codec.Serialise (deserialiseOrFail)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logErrorNS, logInfoNS)
import Control.Monad.Reader.Has (Has, MonadReader, ask)
import Control.Monad.Trans.Except (except, runExceptT, throwE)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Functor.Contravariant ((>$<))
import Data.List (foldl')
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Hasql.Connection (Connection)
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session (Session)
import Hasql.Session qualified as Session
import Hasql.Statement (Statement (..))

import Block.Types (BlockInfo (BlockInfo))
import PlutusData qualified

data Datum = Datum
    { hash :: Text
    , value :: ByteString
    }
    deriving stock (Eq, Show)

getDatumSession :: Text -> Session Datum
getDatumSession datumHash =
    Session.statement datumHash getDatumStatement

getDatumStatement :: Statement Text Datum
getDatumStatement = Statement sql enc dec True
  where
    sql =
        "SELECT hash, value FROM datums WHERE hash = $1"
    enc =
        Encoders.param (Encoders.nonNullable Encoders.text)
    dec =
        Decoders.singleRow $
            Datum
                <$> Decoders.column (Decoders.nonNullable Decoders.text)
                <*> Decoders.column (Decoders.nonNullable Decoders.bytea)

getDatumsSession :: [Text] -> Session (Vector Datum)
getDatumsSession datumHashes =
    Session.statement datumHashes getDatumsStatement

getDatumsStatement :: Statement [Text] (Vector Datum)
getDatumsStatement = Statement sql enc dec True
  where
    sql =
        "SELECT hash, value FROM datums WHERE hash = ANY ($1)"
    enc =
        Encoders.param (Encoders.nonNullable (Encoders.array (Encoders.dimension foldl' (Encoders.element (Encoders.nonNullable Encoders.text)))))
    dec =
        Decoders.rowVector $
            Datum
                <$> Decoders.column (Decoders.nonNullable Decoders.text)
                <*> Decoders.column (Decoders.nonNullable Decoders.bytea)

insertDatumsSession :: [Text] -> [ByteString] -> Session ()
insertDatumsSession datumHashes datumValues = do
    Session.statement (datumHashes, datumValues) insertDatumsStatement

insertDatumsStatement :: Statement ([Text], [ByteString]) ()
insertDatumsStatement = Statement sql enc dec True
  where
    sql =
        "INSERT INTO datums (hash, value) (SELECT h::text, v::bytea FROM unnest($1, $2) AS x(h, v)) ON CONFLICT DO NOTHING"

    encArray elemEncoder =
        Encoders.param (Encoders.nonNullable (Encoders.array (Encoders.dimension foldl' (Encoders.element (Encoders.nonNullable elemEncoder)))))
    enc =
        (fst >$< encArray Encoders.text)
            <> (snd >$< encArray Encoders.bytea)

    dec = Decoders.noResult

initTables :: (MonadIO m, MonadReader r m, Has Connection r) => m ()
initTables = do
    let sql = do
            Session.sql "CREATE TABLE IF NOT EXISTS datums (hash text, value bytea);"
            Session.sql "CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS datums_hash_index ON datums (hash);"
            Session.sql
                "CREATE TABLE IF NOT EXISTS last_block \
                \ (onerow_id bool PRIMARY KEY DEFAULT TRUE, slot integer, hash text, CONSTRAINT onerow CHECK (onerow_id))"
    conn <- ask
    liftIO $ void $ Session.run sql conn

initLastBlock ::
    (MonadIO m, MonadLogger m, MonadReader r m, Has Connection r) => BlockInfo -> m ()
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

updateLastBlock :: (MonadIO m, MonadLogger m, MonadReader r m, Has Connection r) => BlockInfo -> m ()
updateLastBlock (BlockInfo slot hash) = do
    let sql = "UPDATE last_block SET slot = $1, hash = $2"
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
            logErrorNS "updateLastBlock" $ Text.pack $ show err
            pure ()

getLastBlock :: (MonadIO m, MonadLogger m, MonadReader r m, Has Connection r) => m (Maybe BlockInfo)
getLastBlock = do
    let sql = "SELECT slot, hash FROM last_block LIMIT 1"
        enc = Encoders.noParams
        dec =
            Decoders.singleRow $
                BlockInfo
                    <$> Decoders.column (Decoders.nonNullable Decoders.int8)
                    <*> Decoders.column (Decoders.nonNullable Decoders.text)
        stmt = Session.statement () $ Statement sql enc dec True
    dbConnection <- ask
    res <- liftIO $ Session.run stmt dbConnection
    case res of
        Right x -> pure . pure $ x
        Left err -> do
            logErrorNS "getLastBlock" $ Text.pack $ show err
            pure Nothing

data DatabaseError
    = DatabaseErrorDecodeError [ByteString]
    | DatabaseErrorNotFound

toPlutusData :: Datum -> Either DatabaseError PlutusData.Data
toPlutusData datum =
    let res = deserialiseOrFail @PlutusData.Data (BSL.fromStrict $ value datum)
     in case res of
            Left _ -> Left $ DatabaseErrorDecodeError [value datum]
            Right x -> pure x

toPlutusDataMany :: Vector Datum -> Either DatabaseError (Vector (Text, PlutusData.Data))
toPlutusDataMany datums =
    let res = fmap (\d -> (d,) . deserialiseOrFail @PlutusData.Data . BSL.fromStrict . value $ d) datums
        rightToMaybe (Right x) = Just x
        rightToMaybe _ = Nothing
        leftToMaybe (Left x) = Just x
        leftToMaybe _ = Nothing
        correct = Vector.mapMaybe (\(datum, data') -> (hash datum,) <$> rightToMaybe data') res
        faulty = Vector.toList $ Vector.mapMaybe (fmap fst . (\(datum, data') -> (value datum,) <$> leftToMaybe data')) res
     in if null faulty
            then pure correct
            else Left $ DatabaseErrorDecodeError faulty

getDatumByHash ::
    (MonadIO m, MonadReader r m, Has Connection r) =>
    Text ->
    m (Either DatabaseError PlutusData.Data)
getDatumByHash hash = runExceptT $ do
    conn <- ask
    res' <- liftIO (Session.run (getDatumSession hash) conn)
    case res' of
        Left _ -> throwE DatabaseErrorNotFound
        Right datum -> except $ toPlutusData datum

getDatumsByHashes ::
    (MonadIO m, MonadReader r m, Has Connection r) =>
    [Text] ->
    m (Either DatabaseError (Vector (Text, PlutusData.Data)))
getDatumsByHashes hashes = runExceptT $ do
    conn <- ask
    res' <- liftIO (Session.run (getDatumsSession hashes) conn)
    case res' of
        Left _ -> throwE DatabaseErrorNotFound
        Right datums -> except $ toPlutusDataMany datums

saveDatums :: (MonadIO m, MonadLogger m, MonadReader r m, Has Connection r) => [(Text, ByteString)] -> m ()
saveDatums datums = do
    dbConnection <- ask
    let (datumHashes, datumValues) = unzip datums
    logInfoNS "saveDatumsFromAlonzoBlock" $ "Inserting datums: " <> Text.intercalate ", " datumHashes
    res <- liftIO $ Session.run (insertDatumsSession datumHashes datumValues) dbConnection
    case res of
        Right _ -> pure ()
        Left err -> do
            logErrorNS "saveDatumsFromAlonzoBlock" $ "Error inserting datums: " <> Text.pack (show err)
            pure ()
