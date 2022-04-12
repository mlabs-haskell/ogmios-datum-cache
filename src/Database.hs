module Database (
    getDatumSession,
    getDatumsSession,
    Datum (..),
    datumInsertSession,
    insertDatumsSession,
    initTables,
) where

import Data.ByteString (ByteString)
import Data.Functor.Contravariant ((>$<))
import Data.List (foldl')
import Data.Text (Text)
import Data.Vector (Vector)
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session (Session)
import Hasql.Session qualified as Session
import Hasql.Statement (Statement (..))

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

datumInsertSession :: Text -> ByteString -> Session ()
datumInsertSession datumHash datumValue = do
    Session.statement (datumHash, datumValue) datumInsertStatement

datumInsertStatement :: Statement (Text, ByteString) ()
datumInsertStatement = Statement sql enc dec True
  where
    sql =
        "INSERT INTO datums VALUES ($1, $2) ON CONFLICT DO NOTHING"
    enc =
        (fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
            <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.bytea))

    dec =
        Decoders.noResult

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

initTables :: Session ()
initTables = do
    Session.sql "CREATE TABLE IF NOT EXISTS datums (hash text, value bytea);"
    Session.sql "CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS datums_hash_index ON datums (hash);"
