module Api.WebSocket (websocketServer) where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logErrorNS)
import Data.Aeson qualified as Json
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Network.WebSockets qualified as WS

import Api.Types (FirstFetchBlock (FirstFetchBlock))
import Api.WebSocket.Json (
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
    Method (..),
 )
import App (App)
import Block.Fetch (
    StartBlockFetcherError (StartBlockFetcherErrorAlreadyRunning),
    StopBlockFetcherError (StopBlockFetcherErrorNotRunning),
    startBlockFetcher,
    stopBlockFetcher,
 )
import Data.Int (Int64)
import Database (
    DatabaseError (DatabaseErrorDecodeError, DatabaseErrorNotFound),
 )
import Database qualified as Db

getDatumByHash ::
    WS.Connection ->
    Text ->
    App ()
getDatumByHash conn hash = do
    res <- Db.getDatumByHash hash
    case res of
        Left (DatabaseErrorDecodeError _) ->
            sendTextData conn $ mkGetDatumByHashFault "Error deserializing plutus Data"
        Left DatabaseErrorNotFound ->
            sendTextData conn $ mkGetDatumByHashResponse Nothing
        Right datum ->
            sendTextData conn $ mkGetDatumByHashResponse $ Just datum

getDatumsByHashes ::
    WS.Connection ->
    [Text] ->
    App ()
getDatumsByHashes conn hashes = do
    res <- Db.getDatumsByHashes hashes
    case res of
        Left (DatabaseErrorDecodeError faulty) -> do
            let resp = mkGetDatumsByHashesFault $ "Error deserializing plutus Data in: " <> Text.pack (show faulty)
            sendTextData conn resp
        Left DatabaseErrorNotFound ->
            sendTextData conn $ mkGetDatumsByHashesResponse Nothing
        Right datums -> do
            let datums' = Vector.toList $ Vector.map (Json.toJSON . uncurry GetDatumsByHashesDatum) datums
            sendTextData conn $ mkGetDatumsByHashesResponse (Just datums')

getLastBlock :: WS.Connection -> App ()
getLastBlock conn = do
    block' <- Db.getLastBlock
    case block' of
        Just block ->
            sendTextData conn $ mkGetBlockResponse block
        Nothing ->
            sendTextData conn mkGetBlockFault

startFetchBlocks ::
    WS.Connection ->
    Int64 ->
    Text ->
    App ()
startFetchBlocks conn firstBlockSlot firstBlockId = do
    res <- startBlockFetcher $ pure $ FirstFetchBlock firstBlockSlot firstBlockId
    case res of
        Left StartBlockFetcherErrorAlreadyRunning ->
            sendTextData conn $ mkStartFetchBlocksFault "Block fetcher already running"
        Right () ->
            sendTextData conn mkStartFetchBlocksResponse

cancelFetchBlocks ::
    WS.Connection ->
    App ()
cancelFetchBlocks conn = do
    res <- stopBlockFetcher
    case res of
        Left StopBlockFetcherErrorNotRunning ->
            sendTextData conn $ mkCancelFetchBlocksFault "No block fetcher running"
        Right () ->
            sendTextData conn mkCancelFetchBlocksResponse

sendTextData :: (Json.ToJSON a) => WS.Connection -> a -> App ()
sendTextData conn =
    liftIO . WS.sendTextData conn . Json.encode

websocketServer ::
    WS.Connection ->
    App ()
websocketServer conn = forever $ do
    jsonMsg <- receiveData
    case Json.decode @Method jsonMsg of
        Nothing -> do
            logErrorNS "websocketServer" "Error parsing action"
        Just action ->
            case action of
                GetDatumByHash hash ->
                    getDatumByHash conn hash
                GetDatumsByHashes hashes ->
                    getDatumsByHashes conn hashes
                GetBlock ->
                    getLastBlock conn
                StartFetchBlocks firstBlockSlot firstBlockId ->
                    startFetchBlocks conn firstBlockSlot firstBlockId
                CancelFetchBlocks ->
                    cancelFetchBlocks conn
  where
    receiveData = liftIO $ WS.receiveData conn
