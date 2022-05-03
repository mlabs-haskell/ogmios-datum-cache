module Api.Handler (datumServiceHandlers) where

import Control.Monad.Catch (throwM)
import Control.Monad.Logger (logInfoNS)
import Data.Text (Text)
import Network.WebSockets qualified as WS
import Servant
import Servant.API.Generic (ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)

import Api (ControlApi (..), DatumApi (..), Routes (..), WebSocketApi (..))
import Api.Error (throwJsonError)
import Api.Types (
    CancelBlockFetchingResponse (..),
    FirstFetchBlock (FirstFetchBlock),
    GetDatumByHashResponse (..),
    GetDatumsByHashesDatum (..),
    GetDatumsByHashesRequest (..),
    GetDatumsByHashesResponse (..),
    StartBlockFetchingRequest (..),
    StartBlockFetchingResponse (..),
 )
import Api.WebSocket (websocketServer)
import App (App (..))
import Block.Fetch (
    StartBlockFetcherError (StartBlockFetcherErrorAlreadyRunning),
    StopBlockFetcherError (StopBlockFetcherErrorNotRunning),
    startBlockFetcher,
    stopBlockFetcher,
 )
import Database (
    DatabaseError (DatabaseErrorDecodeError, DatabaseErrorNotFound),
 )
import Database qualified as Db

datumServiceHandlers :: Routes (AsServerT App)
datumServiceHandlers = Routes{..}
  where
    datumRoutes :: ToServant DatumApi (AsServerT App)
    datumRoutes = genericServerT DatumApi{..}

    catchDatabaseError r = do
        case r of
            Left (DatabaseErrorDecodeError _) ->
                throwM err500
            Left DatabaseErrorNotFound ->
                throwM err404
            Right x -> pure x

    getDatumByHash :: Text -> App GetDatumByHashResponse
    getDatumByHash hash = do
        datum <- Db.getDatumByHash hash >>= catchDatabaseError
        pure $ GetDatumByHashResponse datum

    getDatumsByHashes :: GetDatumsByHashesRequest -> App GetDatumsByHashesResponse
    getDatumsByHashes (GetDatumsByHashesRequest hashes) = do
        datums <- Db.getDatumsByHashes hashes >>= catchDatabaseError
        pure $ GetDatumsByHashesResponse $ fmap (uncurry GetDatumsByHashesDatum) datums

    getLastBlock :: App FirstFetchBlock
    getLastBlock = do
        block' <- Db.getLastBlock
        case block' of
            Just block -> pure block
            Nothing -> throwM err404

    -- control api
    controlRoutes :: ToServant ControlApi (AsServerT App)
    controlRoutes = genericServerT ControlApi{..}

    startBlockFetching :: StartBlockFetchingRequest -> App StartBlockFetchingResponse
    startBlockFetching (StartBlockFetchingRequest firstBlockSlot firstBlockId) = do
        res <- startBlockFetcher $ pure $ FirstFetchBlock firstBlockSlot firstBlockId
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
    websocketRoutes = genericServerT WebSocketApi{..}

    websocketApi :: WS.Connection -> App ()
    websocketApi conn = do
        logInfoNS "websocketApi" "New WS connection established"
        websocketServer conn
