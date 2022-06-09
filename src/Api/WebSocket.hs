module Api.WebSocket (websocketServer) where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logErrorNS)
import Control.Monad.Reader.Has (ask)
import Data.Aeson qualified as Aeson
import Data.Default (def)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Network.WebSockets qualified as WebSockets

import Api.WebSocket.Json (
  JsonWspFault,
  JsonWspResponse,
  mkCancelFetchBlocksFault,
  mkCancelFetchBlocksResponse,
  mkGetBlockFault,
  mkGetBlockResponse,
  mkGetDatumByHashFault,
  mkGetDatumByHashResponse,
  mkGetDatumsByHashesFault,
  mkGetDatumsByHashesResponse,
  mkHealthcheckResponse,
  mkStartFetchBlocksFault,
  mkStartFetchBlocksResponse,
 )
import Api.WebSocket.Types (
  GetDatumsByHashesDatum (GetDatumsByHashesDatum),
  JsonWspRequest (JsonWspRequest),
  Method (
    CancelFetchBlocks,
    GetBlock,
    GetDatumByHash,
    GetDatumsByHashes,
    GetHealthcheck,
    StartFetchBlocks
  ),
 )
import App.Env (ControlApiToken)
import App.Types (App)
import Block.Fetch (
  startBlockErrMsg,
  startBlockFetcher,
  stopBlockErrMsg,
  stopBlockFetcher,
 )
import Block.Filter (DatumFilter)
import Block.Types (BlockInfo (BlockInfo))
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
  block' <- Database.getLastBlock
  pure $ case block' of
    Nothing ->
      Left mkGetBlockFault
    Just block ->
      Right $ mkGetBlockResponse block

withControlAuthToken :: ControlApiToken -> App WSResponse -> App WSResponse
withControlAuthToken token action = do
  expectToken <- ask
  if expectToken == token
    then action
    else pure $ Left $ mkCancelFetchBlocksFault "Control API token not granted"

startFetchBlocks ::
  Int64 ->
  Text ->
  DatumFilter ->
  ControlApiToken ->
  App WSResponse
startFetchBlocks firstBlockSlot firstBlockId datumFilter token =
  withControlAuthToken token $ do
    res <- startBlockFetcher (BlockInfo firstBlockSlot firstBlockId) datumFilter
    pure $ case res of
      Left err ->
        Left $ mkStartFetchBlocksFault $ startBlockErrMsg err
      Right () ->
        Right mkStartFetchBlocksResponse

cancelFetchBlocks ::
  ControlApiToken ->
  App WSResponse
cancelFetchBlocks token = withControlAuthToken token $ do
  res <- stopBlockFetcher
  pure $ case res of
    Left err ->
      Left $ mkCancelFetchBlocksFault $ stopBlockErrMsg err
    Right () ->
      Right mkCancelFetchBlocksResponse

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
        StartFetchBlocks firstBlockSlot firstBlockId datumFilter' token -> do
          let datumFilter = fromMaybe def datumFilter'
          startFetchBlocks firstBlockSlot firstBlockId datumFilter token
        CancelFetchBlocks token ->
          cancelFetchBlocks token
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
