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
import Control.Monad.Reader (ask)

import qualified PlutusData
import Api
import Api.Types
import App
import App.Env
import qualified Database as Db

datumServiceHandlers :: Routes (AsServerT App)
datumServiceHandlers = Routes{..}
  where
    datumRoutes :: ToServant DatumApi (AsServerT App)
    datumRoutes = genericServerT DatumApi{..}

    toPlutusData :: Db.Datum -> App PlutusData.Data
    toPlutusData datumRes =
      deserialiseOrFail @PlutusData.Data (BSL.fromStrict $ Db.value datumRes) & either (const $ throwError err500) pure

    getDatumByHash :: Text -> App GetDatumByHashResponse
    getDatumByHash hash = do
      Env{..} <- ask
      datumRes <- liftIO (Session.run (Db.getDatumSession hash) envDbConnection) >>= either (const $ throwError err404) pure
      plutusData <- toPlutusData datumRes

      pure $ GetDatumByHashResponse plutusData

    getDatumsByHashes :: GetDatumsByHashesRequest -> App GetDatumsByHashesResponse
    getDatumsByHashes (GetDatumsByHashesRequest hashes) = do
      Env{..} <- ask
      datums <- liftIO (Session.run (Db.getDatumsSession hashes) envDbConnection) >>= either (const $ throwError err404) pure
      plutusDatums <- Vector.mapM (\dt -> GetDatumsByHashesDatum (Db.hash dt) <$> (toPlutusData dt)) datums
      pure $ GetDatumsByHashesResponse plutusDatums

    -- control api
    controlRoutes :: ToServant ControlApi (AsServerT App)
    controlRoutes = genericServerT ControlApi{..}

    addDatumHashes :: AddDatumHashesRequest -> App AddDatumHashesResponse
    addDatumHashes (AddDatumHashesRequest hashes) = do
      liftIO $ print hashes
      pure $ AddDatumHashesResponse "Successfully added hashes"

    removeDatumHashes :: RemoveDatumHashesRequest -> App RemoveDatumHashesResponse
    removeDatumHashes (RemoveDatumHashesRequest hashes) = do
      liftIO $ print hashes
      pure $ RemoveDatumHashesResponse "Successfully removed hashes"

    setDatumHashes :: SetDatumHashesRequest -> App SetDatumHashesResponse
    setDatumHashes (SetDatumHashesRequest hashes) = do
      liftIO $ print hashes
      pure $ SetDatumHashesResponse "Successfully set hashes"
