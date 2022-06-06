module Api.WebSocket (websocketServer) where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logErrorNS)
import Control.Monad.Reader.Has (ask)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Network.WebSockets qualified as WebSockets

import Api.WebSocket.Json (
  JsonWspFault,
  JsonWspResponse,
  mkGetBlockFault,
  mkGetBlockResponse,
  mkGetDatumByHashFault,
  mkGetDatumByHashResponse,
  mkGetDatumsByHashesFault,
  mkGetDatumsByHashesResponse,
  mkHealthcheckResponse,
 )
import Api.WebSocket.Types (
  GetDatumsByHashesDatum (GetDatumsByHashesDatum),
  JsonWspRequest (JsonWspRequest),
  Method (
    GetBlock,
    GetDatumByHash,
    GetDatumsByHashes,
    GetHealthcheck
  ),
 )
import App (App)
import Database (
  DatabaseError (DatabaseErrorDecodeError, DatabaseErrorNotFound),
 )
import Database qualified

type WSResponse =
  Either
    (Maybe Aeson.Value -> JsonWspFault)
    (Maybe Aeson.Value -> JsonWspResponse)

getDatumByHash ::
  Text ->
  App WSResponse
getDatumByHash hash = do
  res <- Database.getDatumByHash hash
  pure $ case res of
    Left (DatabaseErrorDecodeError faulty) ->
      Left $
        mkGetDatumByHashFault $
          "Error deserializing plutus Data in: " <> Text.pack (show faulty)
    Left DatabaseErrorNotFound ->
      Right $ mkGetDatumByHashResponse Nothing
    Right datum ->
      Right $ mkGetDatumByHashResponse $ Just datum

getDatumsByHashes ::
  [Text] ->
  App WSResponse
getDatumsByHashes hashes = do
  res <- Database.getDatumsByHashes hashes
  pure $ case res of
    Left (DatabaseErrorDecodeError faulty) -> do
      let resp =
            mkGetDatumsByHashesFault $
              "Error deserializing plutus Data in: " <> Text.pack (show faulty)
      Left resp
    Left DatabaseErrorNotFound ->
      Right $ mkGetDatumsByHashesResponse Nothing
    Right datums -> do
      let datums' =
            Vector.toList $
              Vector.map (Aeson.toJSON . uncurry GetDatumsByHashesDatum) datums
      Right $ mkGetDatumsByHashesResponse (Just datums')

getLastBlock :: App WSResponse
getLastBlock = do
  dbConn <- ask
  block' <- Database.getLastBlock dbConn
  pure $ case block' of
    Nothing ->
      Left mkGetBlockFault
    Just block ->
      Right $ mkGetBlockResponse block

getHealthcheck :: App WSResponse
getHealthcheck = do
  pure $ Right mkHealthcheckResponse

websocketServer ::
  WebSockets.Connection ->
  App ()
websocketServer conn = forever $ do
  jsonMsg <- receiveData
  case Aeson.decode @JsonWspRequest jsonMsg of
    Nothing ->
      logErrorNS "websocketServer" "Error parsing action"
    Just (JsonWspRequest mirror method) -> do
      response <- case method of
        GetDatumByHash hash ->
          getDatumByHash hash
        GetDatumsByHashes hashes ->
          getDatumsByHashes hashes
        GetBlock ->
          getLastBlock
        GetHealthcheck ->
          getHealthcheck
      let jsonResp =
            either
              (\l -> Aeson.encode $ l mirror)
              (\r -> Aeson.encode $ r mirror)
              response
      sendTextData jsonResp
  where
    sendTextData = liftIO . WebSockets.sendTextData conn
    receiveData = liftIO $ WebSockets.receiveData conn
