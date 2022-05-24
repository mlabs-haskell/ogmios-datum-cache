{-# LANGUAGE NamedFieldPuns #-}

module Api.Handler (
  controlApiAuthCheck,
  datumServiceHandlers,
) where

import Control.Monad.Catch (throwM)
import Control.Monad.Logger (logInfoNS)
import Data.Default (def)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Network.WebSockets qualified as WebSockets
import Servant (err404, err422, err500)
import Servant.API.BasicAuth (BasicAuthData (BasicAuthData))
import Servant.API.Generic (ToServant)
import Servant.Server (
  BasicAuthCheck (BasicAuthCheck),
  BasicAuthResult (Authorized, Unauthorized),
 )
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
  ControlApiAuthData (ControlApiAuthData),
  GetDatumByHashResponse (GetDatumByHashResponse),
  GetDatumsByHashesDatum (GetDatumsByHashesDatum),
  GetDatumsByHashesRequest (GetDatumsByHashesRequest),
  GetDatumsByHashesResponse (GetDatumsByHashesResponse),
  StartBlockFetchingRequest (StartBlockFetchingRequest),
  StartBlockFetchingResponse (StartBlockFetchingResponse),
 )
import Api.WebSocket (websocketServer)
import App (App)
import App.Env (
  ControlApiToken (unControlApiToken),
  Env (Env, envControlApiToken),
 )
import Block.Fetch (
  StartBlockFetcherError (StartBlockFetcherErrorAlreadyRunning),
  StopBlockFetcherError (StopBlockFetcherErrorNotRunning),
  startBlockErrMsg,
  startBlockFetcher,
  stopBlockErrMsg,
  stopBlockFetcher,
 )
import Block.Types (BlockInfo (BlockInfo))
import Database (
  DatabaseError (DatabaseErrorDecodeError, DatabaseErrorNotFound),
 )
import Database qualified

controlApiAuthCheck :: Env -> BasicAuthCheck ControlApiAuthData
controlApiAuthCheck Env {envControlApiToken} =
  BasicAuthCheck $ \(BasicAuthData usr pwd) ->
    let expect = fromString <$> unControlApiToken envControlApiToken
        passed = Just (usr <> ":" <> pwd)
     in pure $
          if expect == passed
            then Authorized ControlApiAuthData
            else Unauthorized

datumServiceHandlers :: Routes (AsServerT App)
datumServiceHandlers = Routes {datumRoutes, controlRoutes, websocketRoutes}
  where
    datumRoutes :: ToServant DatumApi (AsServerT App)
    datumRoutes = genericServerT DatumApi {getDatumByHash, getDatumsByHashes, getLastBlock, getHealthcheck}

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
    controlRoutes :: ControlApiAuthData -> ToServant ControlApi (AsServerT App)
    controlRoutes ControlApiAuthData =
      genericServerT
        ControlApi {startBlockFetching, cancelBlockFetching}

    startBlockFetching ::
      StartBlockFetchingRequest ->
      App StartBlockFetchingResponse
    startBlockFetching
      (StartBlockFetchingRequest firstBlockSlot firstBlockId datumFilter') = do
        let datumFilter = fromMaybe def datumFilter'
        res <- startBlockFetcher (BlockInfo firstBlockSlot firstBlockId) datumFilter
        case res of
          Left err@StartBlockFetcherErrorAlreadyRunning ->
            throwJsonError err422 $ startBlockErrMsg err
          Right () ->
            pure $ StartBlockFetchingResponse "Started block fetcher"

    cancelBlockFetching :: App CancelBlockFetchingResponse
    cancelBlockFetching = do
      res <- stopBlockFetcher
      case res of
        Left err@StopBlockFetcherErrorNotRunning ->
          throwJsonError err422 $ stopBlockErrMsg err
        Right () -> pure $ CancelBlockFetchingResponse "Stopped block fetcher"

    websocketRoutes :: ToServant WebSocketApi (AsServerT App)
    websocketRoutes = genericServerT WebSocketApi {websocketApi}

    websocketApi :: WebSockets.Connection -> App ()
    websocketApi conn = do
      logInfoNS "websocketApi" "New WS connection established"
      websocketServer conn
