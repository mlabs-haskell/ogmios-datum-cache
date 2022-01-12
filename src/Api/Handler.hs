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
import Control.Monad.Catch (throwM)

import qualified PlutusData
import Api
import Api.Types
import App
import App.Env
import qualified App.RequestedDatumHashes as RequestedDatumHashes
import qualified Database as Db

datumServiceHandlers :: Routes (AsServerT App)
datumServiceHandlers = Routes{..}
  where
    datumRoutes :: ToServant DatumApi (AsServerT App)
    datumRoutes = genericServerT DatumApi{..}

    toPlutusData :: Db.Datum -> App PlutusData.Data
    toPlutusData datumRes =
      deserialiseOrFail @PlutusData.Data (BSL.fromStrict $ Db.value datumRes) & either (const $ throwM err500) pure

    getDatumByHash :: Text -> App GetDatumByHashResponse
    getDatumByHash hash = do
      Env{..} <- ask
      datumRes <- liftIO (Session.run (Db.getDatumSession hash) envDbConnection) >>= either (const $ throwM err404) pure
      plutusData <- toPlutusData datumRes

      pure $ GetDatumByHashResponse plutusData

    getDatumsByHashes :: GetDatumsByHashesRequest -> App GetDatumsByHashesResponse
    getDatumsByHashes (GetDatumsByHashesRequest hashes) = do
      Env{..} <- ask
      datums <- liftIO (Session.run (Db.getDatumsSession hashes) envDbConnection) >>= either (const $ throwM err404) pure
      plutusDatums <- Vector.mapM (\dt -> GetDatumsByHashesDatum (Db.hash dt) <$> (toPlutusData dt)) datums
      pure $ GetDatumsByHashesResponse plutusDatums

    -- control api
    controlRoutes :: ToServant ControlApi (AsServerT App)
    controlRoutes = genericServerT ControlApi{..}

    addDatumHashes :: AddDatumHashesRequest -> App AddDatumHashesResponse
    addDatumHashes (AddDatumHashesRequest hashes) = do
      Env{..} <- ask
      RequestedDatumHashes.add hashes envRequestedDatumHashes
      pure $ AddDatumHashesResponse "Successfully added hashes"

    removeDatumHashes :: RemoveDatumHashesRequest -> App RemoveDatumHashesResponse
    removeDatumHashes (RemoveDatumHashesRequest hashes) = do
      Env{..} <- ask
      RequestedDatumHashes.remove hashes envRequestedDatumHashes
      pure $ RemoveDatumHashesResponse "Successfully removed hashes"

    setDatumHashes :: SetDatumHashesRequest -> App SetDatumHashesResponse
    setDatumHashes (SetDatumHashesRequest hashes) = do
      Env{..} <- ask
      RequestedDatumHashes.set hashes envRequestedDatumHashes
      pure $ SetDatumHashesResponse "Successfully set hashes"

    getDatumHashes :: App GetDatumHashesResponse
    getDatumHashes = do
      Env{..} <- ask
      hashSet <- RequestedDatumHashes.get envRequestedDatumHashes
      pure $ GetDatumHashesResponse hashSet
