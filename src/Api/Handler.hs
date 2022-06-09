module Api.Handler (cacheServiceHandlers) where

import Control.Monad.Catch (throwM)
import Control.Monad.Logger (logInfoNS)
import Data.Default (def)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Network.WebSockets qualified as WebSockets
import Servant (err404, err422, err500)
import Servant.API.Generic (ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)

import Api (
  ControlApi (ControlApi, cancelBlockFetching, startBlockFetching),
  CacheApi (CacheApi, getDatumByHash, getDatumsByHashes, getHealthcheck, getLastBlock),
  Routes (Routes, controlRoutes, cacheRoutes, websocketRoutes),
  WebSocketApi (WebSocketApi, websocketApi),
 )
import Api.Error (JsonError (JsonError), throwJsonError)
import Api.Types (
  CancelBlockFetchingResponse (CancelBlockFetchingResponse),
  GetDatumByHashResponse (GetDatumByHashResponse),
  GetDatumsByHashesDatum (GetDatumsByHashesDatum),
  GetDatumsByHashesRequest (GetDatumsByHashesRequest),
  GetDatumsByHashesResponse (GetDatumsByHashesResponse),
  StartBlockFetchingRequest (StartBlockFetchingRequest),
  StartBlockFetchingResponse (StartBlockFetchingResponse),
 )
import Api.WebSocket (websocketServer)
import App.Types (App)
import Block.Fetch (
  StartBlockFetcherError (StartBlockFetcherErrorAlreadyRunning),
  StopBlockFetcherError (StopBlockFetcherErrorNotRunning),
  startBlockFetcher,
  stopBlockFetcher,
 )
import Block.Types (BlockInfo (BlockInfo))
import Database (
  DatabaseError (DatabaseErrorDecodeError, DatabaseErrorNotFound),
 )
import Database qualified
import Block.Filter (DatumFilter, TxFilter)

cacheServiceHandlers :: Routes (AsServerT App)
cacheServiceHandlers = Routes {..}
  where
    cacheRoutes :: ToServant CacheApi (AsServerT App)
    cacheRoutes = genericServerT CacheApi {..}

    catchDatabaseError r = do
      case r of
        Left (DatabaseErrorDecodeError e) ->
          throwJsonError err500 $
            JsonError $ Text.pack $ "Decoding error: " <> show e
        Left DatabaseErrorNotFound ->
          throwM err404
        Right x -> pure x

    getDatumByHash :: Text -> App GetDatumByHashResponse
    getDatumByHash hash = do
      datum <- Database.getDatumByHash hash >>= catchDatabaseError
      pure $ GetDatumByHashResponse datum

    getDatumsByHashes ::
      GetDatumsByHashesRequest ->
      App GetDatumsByHashesResponse
    getDatumsByHashes (GetDatumsByHashesRequest hashes) = do
      datums <- Database.getDatumsByHashes hashes >>= catchDatabaseError
      pure $
        GetDatumsByHashesResponse $
          fmap (uncurry GetDatumsByHashesDatum) datums

    getLastBlock :: App BlockInfo
    getLastBlock = do
      block' <- Database.getLastBlock
      case block' of
        Just block -> pure block
        Nothing -> throwM err404

    getHealthcheck :: App ()
    getHealthcheck = pure ()

    -- control api
    controlRoutes :: ToServant ControlApi (AsServerT App)
    controlRoutes = genericServerT ControlApi {..}

    startBlockFetching ::
      StartBlockFetchingRequest ->
      App StartBlockFetchingResponse
    startBlockFetching (StartBlockFetchingRequest firstBlockSlot firstBlockId datumFilter') = do
      let datumFilter = fromMaybe def datumFilter'
          txFilter = txFilterFromDatumFilter datumFilter
      res <- startBlockFetcher (BlockInfo firstBlockSlot firstBlockId) txFilter
      case res of
        Left StartBlockFetcherErrorAlreadyRunning ->
          throwJsonError err422 "Block fetcher already running"
        Right () ->
          pure $ StartBlockFetchingResponse "Started block fetcher"

    cancelBlockFetching :: App CancelBlockFetchingResponse
    cancelBlockFetching = do
      res <- stopBlockFetcher
      case res of
        Left StopBlockFetcherErrorNotRunning ->
          throwJsonError err422 "No block fetcher running"
        Right () ->
          pure $ CancelBlockFetchingResponse "Stopped block fetcher"

    websocketRoutes :: ToServant WebSocketApi (AsServerT App)
    websocketRoutes = genericServerT WebSocketApi {..}

    websocketApi :: WebSockets.Connection -> App ()
    websocketApi conn = do
      logInfoNS "websocketApi" "New WS connection established"
      websocketServer conn

txFilterFromDatumFilter :: DatumFilter -> TxFilter
txFilterFromDatumFilter = error "not implemented"
