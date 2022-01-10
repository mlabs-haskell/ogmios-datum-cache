module Api.Handler where

import Servant
import Servant.API.Generic (ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)
import Data.Text (Text)
import qualified Hasql.Connection as Hasql
import qualified Hasql.Session as Session
import qualified Data.ByteString.Lazy as BSL
import Codec.Serialise (Serialise, deserialiseOrFail)
import Data.Function ((&))
import Control.Monad.IO.Class (liftIO)

import qualified PlutusData
import Api
import Database (getDatumSession, Datum (value))

datumServiceHandlers :: Hasql.Connection -> Routes (AsServerT Handler)
datumServiceHandlers pgConn = Routes{..}
  where
    datumRoutes :: ToServant DatumApi (AsServerT Handler)
    datumRoutes = genericServerT DatumApi{..}

    getDatumByHash :: Text -> Handler GetDatumByHashResponse
    getDatumByHash hash = do
      datumRes <- liftIO (Session.run (getDatumSession hash) pgConn) >>= either (const $ throwError err404) pure
      plutusData <-
        deserialiseOrFail @PlutusData.Data (BSL.fromStrict $ value datumRes) & either (const $ throwError err500) pure
      pure $ GetDatumByHashResponse plutusData

    getDatumsByHashes :: Text -> Handler GetDatumsByHashesResponse
    getDatumsByHashes hashes = do
      pure $ GetDatumsByHashesResponse [PlutusData.I 100, PlutusData.I 200]
