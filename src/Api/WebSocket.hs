module Api.WebSocket (websocketServer) where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logErrorNS)
import Data.Aeson (Value (Null))
import Data.Aeson qualified as Json
import Data.HashMap.Strict qualified as HM
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Network.WebSockets qualified as WS

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
    mkStartFetchBlocksFault,
    mkStartFetchBlocksResponse,
 )
import Api.WebSocket.Types (
    GetDatumsByHashesDatum (..),
    JsonWspRequest (..),
    Method (..),
 )
import App (App)
import Block.Fetch (
    StartBlockFetcherError (StartBlockFetcherErrorAlreadyRunning),
    StopBlockFetcherError (StopBlockFetcherErrorNotRunning),
    startBlockFetcher,
    stopBlockFetcher,
 )
import Block.Filter (DatumFilter)
import Block.Types (BlockInfo (BlockInfo))
import Database (
    DatabaseError (DatabaseErrorDecodeError, DatabaseErrorNotFound),
 )
import Database qualified as Db

getDatumByHash ::
    Text ->
    App (Either JsonWspFault JsonWspResponse)
getDatumByHash hash = do
    res <- Db.getDatumByHash hash
    case res of
        Left (DatabaseErrorDecodeError faulty) ->
            pure $ Left $ mkGetDatumByHashFault $ "Error deserializing plutus Data in: " <> Text.pack (show faulty)
        Left DatabaseErrorNotFound ->
            pure $ Right $ mkGetDatumByHashResponse Nothing
        Right datum ->
            pure $ Right $ mkGetDatumByHashResponse $ Just datum

getDatumsByHashes ::
    [Text] ->
    App (Either JsonWspFault JsonWspResponse)
getDatumsByHashes hashes = do
    res <- Db.getDatumsByHashes hashes
    case res of
        Left (DatabaseErrorDecodeError faulty) -> do
            let resp = mkGetDatumsByHashesFault $ "Error deserializing plutus Data in: " <> Text.pack (show faulty)
            pure $ Left resp
        Left DatabaseErrorNotFound ->
            pure $ Right $ mkGetDatumsByHashesResponse Nothing
        Right datums -> do
            let datums' = Vector.toList $ Vector.map (Json.toJSON . uncurry GetDatumsByHashesDatum) datums
            pure $ Right $ mkGetDatumsByHashesResponse (Just datums')

getLastBlock :: App (Either JsonWspFault JsonWspResponse)
getLastBlock = do
    block' <- Db.getLastBlock
    case block' of
        Nothing ->
            pure $ Left mkGetBlockFault
        Just block ->
            pure $ Right $ mkGetBlockResponse block

startFetchBlocks ::
    Int64 ->
    Text ->
    DatumFilter ->
    App (Either JsonWspFault JsonWspResponse)
startFetchBlocks firstBlockSlot firstBlockId datumFilter = do
    res <- startBlockFetcher (BlockInfo firstBlockSlot firstBlockId) datumFilter
    case res of
        Left StartBlockFetcherErrorAlreadyRunning ->
            pure $ Left $ mkStartFetchBlocksFault "Block fetcher already running"
        Right () ->
            pure $ Right $ mkStartFetchBlocksResponse

cancelFetchBlocks ::
    App (Either JsonWspFault JsonWspResponse)
cancelFetchBlocks = do
    res <- stopBlockFetcher
    case res of
        Left StopBlockFetcherErrorNotRunning ->
            pure $ Left $ mkCancelFetchBlocksFault "No block fetcher running"
        Right () ->
            pure $ Right mkCancelFetchBlocksResponse

websocketServer ::
    WS.Connection ->
    App ()
websocketServer conn = forever $ do
    jsonMsg <- receiveData
    case Json.decode @JsonWspRequest jsonMsg of
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
                StartFetchBlocks firstBlockSlot firstBlockId datumFilter ->
                    startFetchBlocks firstBlockSlot firstBlockId datumFilter
                CancelFetchBlocks ->
                    cancelFetchBlocks
            let jsonResp = either Json.toJSON Json.toJSON response
            sendTextData $ appendJsonWspReflection mirror jsonResp
  where
    sendTextData = liftIO . WS.sendTextData conn . Json.encode

    receiveData = liftIO $ WS.receiveData conn

    -- A little bit hacky but ok
    appendJsonWspReflection mirror = \case
        Json.Object jsonWspResponseObject ->
            Json.Object $ HM.insert "reflection" (fromMaybe Null mirror) jsonWspResponseObject
        -- unreachable: this should not be the case anyway
        nonObject -> nonObject
