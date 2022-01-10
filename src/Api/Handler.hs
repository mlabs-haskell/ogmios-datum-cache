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
import Database (getDatumSession, Datum (value), getDatumsSession)

datumServiceHandlers :: Hasql.Connection -> Routes (AsServerT Handler)
datumServiceHandlers pgConn = Routes{..}
  where
    datumRoutes :: ToServant DatumApi (AsServerT Handler)
    datumRoutes = genericServerT DatumApi{..}

    toPlutusData datumRes =
      deserialiseOrFail @PlutusData.Data (BSL.fromStrict $ value datumRes) & either (const $ throwError err500) pure

    getDatumByHash :: Text -> Handler GetDatumByHashResponse
    getDatumByHash hash = do
      datumRes <- liftIO (Session.run (getDatumSession hash) pgConn) >>= either (const $ throwError err404) pure
      plutusData <- toPlutusData datumRes

      pure $ GetDatumByHashResponse plutusData

    getDatumsByHashes :: Text -> Handler GetDatumsByHashesResponse
    getDatumsByHashes _hashes = do
      let hashes = ["923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"]
      datums <- liftIO (Session.run (getDatumsSession hashes) pgConn) >>= either (const $ throwError err404) pure
      plutusDatums <- Vector.mapM toPlutusData datums
      pure $ GetDatumsByHashesResponse plutusDatums
