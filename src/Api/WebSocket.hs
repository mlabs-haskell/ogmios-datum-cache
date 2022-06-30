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
  JsonWspFault (JsonWspFault),
  JsonWspResponse,
  mkGetBlockFault,
  mkGetBlockResponse,
  mkGetDatumByHashFault,
  mkGetDatumByHashResponse,
  mkGetDatumsByHashesFault,
  mkGetDatumsByHashesResponse,
  mkGetTxByHashResponse,
  mkGetTxByHashResponseFault,
  mkHealthcheckResponse,
  mkSetDatumFilterResponse,
  mkSetStartingBlockFault,
  mkSetStartingBlockResponse,
 )
import Api.WebSocket.Types (
  GetDatumsByHashesDatum (GetDatumsByHashesDatum),
  JsonWspRequest (JsonWspRequest),
  Method (
    GetBlock,
    GetDatumByHash,
    GetDatumsByHashes,
    GetHealthcheck,
    GetTxByHash,
    SetDatumFilter,
    SetStartingBlock
  ),
 )
import App.Env (ControlApiToken)
import App.Types (App)
import Block.Fetch (changeDatumFilter, changeStartingBlock)
import Block.Filter (DatumFilter)
import Block.Types (StartingBlock)
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

getTxByHash ::
  Text ->
  App WSResponse
getTxByHash txId = do
  res <- Database.getTxByHash txId
  pure $ case res of
    Left (DatabaseErrorDecodeError faulty) ->
      Left $
        mkGetTxByHashResponseFault $
          "Error deserializing data from db: " <> Text.pack (show faulty)
    Left DatabaseErrorNotFound ->
      Right $ mkGetTxByHashResponse Nothing
    Right datum ->
      Right $ mkGetTxByHashResponse $ Just datum

getLastBlock :: App WSResponse
getLastBlock = do
  dbConn <- ask
  block' <- Database.getLastBlock dbConn
  pure $ case block' of
    Nothing ->
      Left mkGetBlockFault
    Just block ->
      Right $ mkGetBlockResponse block

withControlAuthToken :: ControlApiToken -> Text -> App WSResponse -> App WSResponse
withControlAuthToken token methodName action = do
  expectToken <- ask
  if expectToken == token
    then action
    else pure $ Left $ JsonWspFault methodName "Control API token not granted" ""

getHealthcheck :: App WSResponse
getHealthcheck = do
  pure $ Right mkHealthcheckResponse

setStartingBlock :: StartingBlock -> App WSResponse
setStartingBlock startingBlock = do
  intersection' <- changeStartingBlock startingBlock
  pure $ case intersection' of
    Nothing ->
      Left mkSetStartingBlockFault
    Just x ->
      Right $ mkSetStartingBlockResponse x

setDatumFilter :: DatumFilter -> App WSResponse
setDatumFilter datumFilter = do
  changeDatumFilter datumFilter
  pure $ Right mkSetDatumFilterResponse

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
        GetTxByHash txId ->
          getTxByHash txId
        GetBlock ->
          getLastBlock
        GetHealthcheck ->
          getHealthcheck
        SetStartingBlock token block ->
          withControlAuthToken token "SetStartingBlock" $ setStartingBlock block
        SetDatumFilter token datumFilter ->
          withControlAuthToken token "SetDatumFilter" $ setDatumFilter datumFilter

      let jsonResp =
            either
              (\l -> Aeson.encode $ l mirror)
              (\r -> Aeson.encode $ r mirror)
              response
      sendTextData jsonResp
  where
    sendTextData = liftIO . WebSockets.sendTextData conn
    receiveData = liftIO $ WebSockets.receiveData conn
