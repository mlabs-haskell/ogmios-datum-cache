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
import qualified Data.Vector as Vector

import qualified PlutusData
import Api
import Api.Types
import qualified Database as Db

datumServiceHandlers :: Hasql.Connection -> Routes (AsServerT Handler)
datumServiceHandlers pgConn = Routes{..}
  where
    datumRoutes :: ToServant DatumApi (AsServerT Handler)
    datumRoutes = genericServerT DatumApi{..}

    toPlutusData :: Db.Datum -> Handler PlutusData.Data
    toPlutusData datumRes =
      deserialiseOrFail @PlutusData.Data (BSL.fromStrict $ Db.value datumRes) & either (const $ throwError err500) pure

    getDatumByHash :: Text -> Handler GetDatumByHashResponse
    getDatumByHash hash = do
      datumRes <- liftIO (Session.run (Db.getDatumSession hash) pgConn) >>= either (const $ throwError err404) pure
      plutusData <- toPlutusData datumRes

      pure $ GetDatumByHashResponse plutusData

    getDatumsByHashes :: GetDatumsByHashesRequest -> Handler GetDatumsByHashesResponse
    getDatumsByHashes (GetDatumsByHashesRequest hashes) = do
      datums <- liftIO (Session.run (Db.getDatumsSession hashes) pgConn) >>= either (const $ throwError err404) pure
      plutusDatums <- Vector.mapM (\dt -> GetDatumsByHashesDatum (Db.hash dt) <$> (toPlutusData dt)) datums
      pure $ GetDatumsByHashesResponse plutusDatums
