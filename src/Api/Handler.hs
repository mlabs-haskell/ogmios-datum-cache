module Api.Handler (datumServiceHandlers) where

import Control.Monad.Catch (throwM)
import Control.Monad.Logger (logInfoNS)
import Data.Text (Text)
import Data.Text qualified as Text
import Network.WebSockets qualified as WebSockets
import Servant
import Servant.API.Generic (ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)

import Api (
  ControlApi (ControlApi, cancelBlockFetching, startBlockFetching),
  DatumApi (DatumApi, getDatumByHash, getDatumsByHashes, getHealthcheck, getLastBlock),
  Routes (Routes, controlRoutes, datumRoutes, websocketRoutes),
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
import App (App)
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

datumServiceHandlers :: Routes (AsServerT App)
datumServiceHandlers = Routes {..}
  where
    datumRoutes :: ToServant DatumApi (AsServerT App)
    datumRoutes = genericServerT DatumApi {..}

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
    startBlockFetching (StartBlockFetchingRequest firstBlockSlot firstBlockId datumFilter) = do
      res <- startBlockFetcher (BlockInfo firstBlockSlot firstBlockId) datumFilter
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
