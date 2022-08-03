{-# LANGUAGE NamedFieldPuns #-}

module Api.Handler (
  controlApiAuthCheck,
  datumServiceHandlers,
) where

import Control.Monad.Catch (throwM)
import Control.Monad.Logger (logInfoNS)
import Data.Map qualified as Map
import Data.String.ToString (toString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Network.WebSockets qualified as WebSockets
import Servant (err404, err500)
import Servant.API.BasicAuth (BasicAuthData (BasicAuthData))
import Servant.API.Generic (ToServant)
import Servant.Server (
  BasicAuthCheck (BasicAuthCheck),
  BasicAuthResult (Authorized, Unauthorized),
 )
import Servant.Server.Generic (AsServerT, genericServerT)

import Api (
  ControlApi (ControlApi, setDatumFilter, setStartingBlock),
  DatumApi (DatumApi, getDatumByHash, getDatumsByHashes, getHealthcheck, getLastBlock, getTx),
  Routes (Routes, controlRoutes, datumRoutes, websocketRoutes),
  WebSocketApi (WebSocketApi, websocketApi),
 )
import Api.Error (JsonError (JsonError), throwJsonError)
import Api.Types (
  ControlApiAuthData (ControlApiAuthData),
  GetDatumByHashResponse (GetDatumByHashResponse),
  GetDatumsByHashesDatum (GetDatumsByHashesDatum),
  GetDatumsByHashesRequest (GetDatumsByHashesRequest),
  GetDatumsByHashesResponse (GetDatumsByHashesResponse),
  SetDatumFilterRequest (SetDatumFilterRequest),
  SetStartingBlockRequest (SetStartingBlockRequest),
 )
import Api.WebSocket (websocketServer)
import App.Env (ControlApiToken (ControlApiToken), Env (Env, envControlApiToken))
import App.Types (App)
import Block.Fetch (changeDatumFilter, changeStartingBlock)
import Block.Types (BlockInfo, CursorPoint, getRawTx)
import Control.Monad.Reader.Has (ask)
import DataHash (DataHash (DataHash))
import Database (
  DatabaseError (DatabaseErrorDecodeError, DatabaseErrorNotFound),
 )
import Database qualified

controlApiAuthCheck :: Env -> BasicAuthCheck ControlApiAuthData
controlApiAuthCheck Env {envControlApiToken} =
  BasicAuthCheck $ \(BasicAuthData usr pwd) -> do
    let expect = envControlApiToken
        passed = ControlApiToken $ toString (usr <> ":" <> pwd)
    pure $
      if expect == passed
        then Authorized ControlApiAuthData
        else Unauthorized

datumServiceHandlers :: Routes (AsServerT App)
datumServiceHandlers =
  Routes {datumRoutes, controlRoutes, websocketRoutes}
  where
    datumRoutes :: ToServant DatumApi (AsServerT App)
    datumRoutes =
      genericServerT
        DatumApi
          { getDatumByHash
          , getDatumsByHashes
          , getLastBlock
          , getHealthcheck
          , getTx
          }

    catchDatabaseError r = do
      case r of
        Left (DatabaseErrorDecodeError value err) ->
          throwJsonError err500 $
            JsonError $
              "Decoding error: " <> Text.pack (show value)
                <> " error: "
                <> Text.pack (show err)
        Left DatabaseErrorNotFound ->
          throwM err404
        Right x -> pure x

    getDatumByHash :: Text -> App GetDatumByHashResponse
    getDatumByHash hash = do
      datum <- Database.getDatumByHash (DataHash hash) >>= catchDatabaseError
      pure $ GetDatumByHashResponse datum

    getDatumsByHashes ::
      GetDatumsByHashesRequest ->
      App GetDatumsByHashesResponse
    getDatumsByHashes (GetDatumsByHashesRequest hashes) = do
      datums <- Database.getDatumsByHashes hashes >>= catchDatabaseError
      let (_, rightDatums) = Map.mapEither id datums
      pure $
        GetDatumsByHashesResponse
          (uncurry GetDatumsByHashesDatum <$> (Vector.fromList . Map.toList) rightDatums)

    getTx :: Text -> App Text
    getTx txId = do
      tx <- Database.getTxByHash txId >>= catchDatabaseError
      pure $ getRawTx tx

    getLastBlock :: App BlockInfo
    getLastBlock = do
      dbConn <- ask
      block' <- Database.getLastBlock dbConn
      case block' of
        Just block -> pure block
        Nothing -> throwM err404

    getHealthcheck :: App ()
    getHealthcheck = pure ()

    setStartingBlock :: SetStartingBlockRequest -> App CursorPoint
    setStartingBlock (SetStartingBlockRequest blockInfo) = do
      intersection' <- changeStartingBlock blockInfo
      case intersection' of
        Nothing -> throwM err404
        Just x -> pure x

    setDatumFilter :: SetDatumFilterRequest -> App ()
    setDatumFilter (SetDatumFilterRequest datumFilter) = do
      changeDatumFilter datumFilter

    controlRoutes :: ControlApiAuthData -> ToServant ControlApi (AsServerT App)
    controlRoutes ControlApiAuthData =
      genericServerT
        ControlApi {setStartingBlock, setDatumFilter}

    websocketRoutes :: ToServant WebSocketApi (AsServerT App)
    websocketRoutes = genericServerT WebSocketApi {websocketApi}

    websocketApi :: WebSockets.Connection -> App ()
    websocketApi conn = do
      logInfoNS "websocketApi" "New WS connection established"
      websocketServer conn
