module Api.Handler (datumServiceHandlers) where

import Control.Monad.Catch (throwM)
import Control.Monad.Logger (logInfoNS)
import Data.Text (Text)
import Data.Text qualified as Text
import Network.WebSockets qualified as WebSockets
import Servant (err404, err500)
import Servant.API.Generic (ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)

import Api (
  ControlApi (ControlApi),
  DatumApi (DatumApi, getDatumByHash, getDatumsByHashes, getHealthcheck, getLastBlock),
  Routes (Routes, controlRoutes, datumRoutes, websocketRoutes),
  WebSocketApi (WebSocketApi, websocketApi),
 )
import Api.Error (JsonError (JsonError), throwJsonError)
import Api.Types (
  GetDatumByHashResponse (GetDatumByHashResponse),
  GetDatumsByHashesDatum (GetDatumsByHashesDatum),
  GetDatumsByHashesRequest (GetDatumsByHashesRequest),
  GetDatumsByHashesResponse (GetDatumsByHashesResponse),
  SetDatumFilterRequest (SetDatumFilterRequest),
  SetStartingBlockRequest (SetStartingBlockRequest),
 )
import Api.WebSocket (websocketServer)
import App.Types (App)
import Block.Fetch (changeDatumFilter, changeStartingBlock)
import Block.Types (BlockInfo, CursorPoint)
import Control.Monad.Reader.Has (ask)
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
      dbConn <- ask
      block' <- Database.getLastBlock dbConn
      case block' of
        Just block -> pure block
        Nothing -> throwM err404

    getHealthcheck :: App ()
    getHealthcheck = pure ()

    -- control api
    controlRoutes :: ToServant ControlApi (AsServerT App)
    controlRoutes = genericServerT $ ControlApi setStartingBlock setDatumFilter

    setStartingBlock :: SetStartingBlockRequest -> App CursorPoint
    setStartingBlock (SetStartingBlockRequest blockInfo) = do
      intersection' <- changeStartingBlock blockInfo
      case intersection' of
        Nothing -> throwM err404
        Just x -> pure x

    setDatumFilter :: SetDatumFilterRequest -> App ()
    setDatumFilter (SetDatumFilterRequest datumFilter) = do
      changeDatumFilter datumFilter

    websocketRoutes :: ToServant WebSocketApi (AsServerT App)
    websocketRoutes = genericServerT WebSocketApi {..}

    websocketApi :: WebSockets.Connection -> App ()
    websocketApi conn = do
      logInfoNS "websocketApi" "New WS connection established"
      websocketServer conn
