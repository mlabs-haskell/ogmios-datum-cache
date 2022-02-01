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
import qualified Data.Text as Text

import qualified Network.WebSockets  as WS
import qualified UnliftIO.Async as Async
import UnliftIO.Exception (onException)
import Control.Monad.Reader (runReaderT)
import Colog (logWarning, logInfo, logError)
import UnliftIO.MVar (tryTakeMVar, isEmptyMVar, tryPutMVar)
import UnliftIO.Concurrent (threadDelay)
import Control.Monad (when, unless, void)

import qualified Data.Aeson as Json

import qualified PlutusData
import Api
import Api.Types
import App
import App.Env
import qualified App.RequestedDatumHashes as RequestedDatumHashes
import qualified Database as Db
import Api.Error (JsonError(..), throwJsonError)
import Api.WebSocket (websocketServer)

import Block.Fetch (wsApp)

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

    startBlockFetching :: StartBlockFetchingRequest -> App StartBlockFetchingResponse
    startBlockFetching (StartBlockFetchingRequest firstBlockSlot firstBlockId) = do
      env@Env{..} <- ask

      isOgmiosWorkerRunning <- not <$> isEmptyMVar envOgmiosWorker
      when isOgmiosWorkerRunning $ do
        throwJsonError err422 (JsonError "Block fetcher already running")

      let runOgmiosClient =
            WS.runClient envOgmiosAddress envOgmiosPort "" $ \wsConn ->
              runReaderT (unApp $ wsApp wsConn (Just (firstBlockSlot, firstBlockId))) env

      ogmiosWorker <- Async.async $ do
        logInfo "Starting ogmios client"
        (liftIO runOgmiosClient) `onException` (do
          logError $ "Error starting ogmios client"
          void $ tryTakeMVar envOgmiosWorker)

      putSuccessful <- tryPutMVar envOgmiosWorker ogmiosWorker
      unless putSuccessful $ do
        Async.cancel ogmiosWorker
        logWarning "Another block fetcher was already running, cancelling worker thread"
        throwJsonError err422 (JsonError "Another block fetcher was already running, cancelling worker thread")

      pure $ StartBlockFetchingResponse "Started block fetcher"

    cancelBlockFetching :: App CancelBlockFetchingResponse
    cancelBlockFetching = do
      Env{..} <- ask
      ogmiosWorker <- tryTakeMVar envOgmiosWorker >>=
        maybe (throwJsonError err422 (JsonError "No block fetcher running")) pure
      Async.cancel ogmiosWorker
      pure $ CancelBlockFetchingResponse "Stopped block fetcher"

    websocketRoutes :: ToServant WebSocketApi (AsServerT App)
    websocketRoutes = genericServerT WebSocketApi{..}

    websocketApi :: WS.Connection -> App ()
    websocketApi conn = do
      logInfo "New WS connection established"
      websocketServer conn
