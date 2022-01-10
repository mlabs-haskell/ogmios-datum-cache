module Database where

import Data.Text (Text)
import qualified Hasql.Session as Session
import Hasql.Session (Session)
import Hasql.Statement (Statement(..))
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Data.Int (Int64)
import Data.Functor.Contravariant ((>$<))
import Data.ByteString (ByteString)

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
    dec = Decoders.singleRow $
      Datum <$>
      Decoders.column (Decoders.nonNullable Decoders.text) <*>
      Decoders.column (Decoders.nonNullable Decoders.bytea)

datumInsertSession :: Text -> ByteString -> Session ()
datumInsertSession datumHash datumValue = do
  Session.statement (datumHash, datumValue) datumInsertStatement

datumInsertStatement :: Statement (Text, ByteString) ()
datumInsertStatement = Statement sql enc dec True
  where
    sql =
      "INSERT INTO datums VALUES ($1, $2) ON CONFLICT DO NOTHING"
    enc =
      (fst >$< Encoders.param (Encoders.nonNullable Encoders.text)) <>
      (snd >$< Encoders.param (Encoders.nonNullable Encoders.bytea))

    dec =
      Decoders.noResult
