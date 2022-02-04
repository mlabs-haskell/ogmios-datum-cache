module Api.WebSocket (websocketServer) where

import Codec.Serialise qualified as Cbor
import Colog (logError, logInfo, logWarning)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, runReaderT)
import Data.Aeson qualified as Json
import Data.ByteString.Lazy qualified as BSL
import Data.Either (fromLeft, fromRight, isLeft)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Hasql.Session qualified as Session
import Network.WebSockets qualified as WS
import UnliftIO.Async qualified as Async
import UnliftIO.Exception (onException)
import UnliftIO.MVar (isEmptyMVar, tryPutMVar, tryTakeMVar)

import Api.WebSocket.Json (
    mkCancelFetchBlocksFault,
    mkCancelFetchBlocksResponse,
    mkDatumFilterAddHashesResponse,
    mkDatumFilterGetHashesResponse,
    mkDatumFilterRemoveHashesResponse,
    mkDatumFilterSetHashesResponse,
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
import App (App (..))
import App.Env (Env (..))
import App.RequestedDatumHashes qualified as RequestedDatumHashes
import Block.Fetch (wsApp)
import Database qualified as Db
import PlutusData qualified

toPlutusData :: Db.Datum -> Either Cbor.DeserialiseFailure PlutusData.Data
toPlutusData dt = Cbor.deserialiseOrFail @PlutusData.Data (BSL.fromStrict $ Db.value dt)

getDatumByHash :: WS.Connection -> Text -> App ()
getDatumByHash conn hash = do
    Env{..} <- ask
    datumRes <- liftIO (Session.run (Db.getDatumSession hash) envDbConnection)
    case datumRes of
        Left _ -> do
            -- TODO: different response?
            let resp = mkGetDatumByHashResponse Nothing
            sendTextData $ Json.encode resp
        Right datum ->
            case toPlutusData datum of
                Left _ -> do
                    let resp = mkGetDatumByHashFault "Error deserializing plutus Data"
                    sendTextData $ Json.encode resp
                Right plutusData -> do
                    let plutusDataJson = Json.toJSON plutusData
                    let resp = mkGetDatumByHashResponse (Just plutusDataJson)
                    sendTextData $ Json.encode resp
  where
    sendTextData = liftIO . WS.sendTextData conn

getDatumsByHashes :: WS.Connection -> [Text] -> App ()
getDatumsByHashes conn hashes = do
    Env{..} <- ask
    datumsRes <- liftIO (Session.run (Db.getDatumsSession hashes) envDbConnection)
    case datumsRes of
        Left _ -> do
            -- TODO: different response?
            let resp = mkGetDatumsByHashesResponse Nothing
            sendTextData $ Json.encode resp
        Right datums -> do
            let toPlutusDataOrErr :: Db.Datum -> Either Text GetDatumsByHashesDatum
                toPlutusDataOrErr dt =
                    case toPlutusData dt of
                        Left _ -> Left $ Db.hash dt
                        Right plutusData -> Right $ GetDatumsByHashesDatum (Db.hash dt) plutusData

            case Vector.partition isLeft $ Vector.map toPlutusDataOrErr datums of
                (linvalidDatums, rvalidDatums) | Vector.null linvalidDatums -> do
                    let plutusDataJson = Vector.toList $ Vector.map (fromRight Json.Null . (Json.toJSON <$>)) rvalidDatums
                    let resp = mkGetDatumsByHashesResponse (Just plutusDataJson)
                    sendTextData $ Json.encode resp
                (linvalidDatums :: Vector.Vector (Either Text GetDatumsByHashesDatum), _) -> do
                    let resp = mkGetDatumsByHashesFault $ "Error deserializing plutus Data in: " <> Text.pack (show $ Vector.toList $ Vector.map (fromLeft "") linvalidDatums)
                    sendTextData $ Json.encode resp
  where
    sendTextData = liftIO . WS.sendTextData conn

startFetchBlocks :: WS.Connection -> Integer -> Text -> App ()
startFetchBlocks conn firstBlockSlot firstBlockId = do
    env@Env{..} <- ask
    isOgmiosWorkerRunning <- not <$> isEmptyMVar envOgmiosWorker
    if isOgmiosWorkerRunning
        then do
            let resp = mkStartFetchBlocksFault "Block fetcher already running"
            sendTextData $ Json.encode resp
        else do
            let runOgmiosClient =
                    WS.runClient envOgmiosAddress envOgmiosPort "" $ \wsConn ->
                        runReaderT (unApp $ wsApp wsConn (Just (firstBlockSlot, firstBlockId))) env

            ogmiosWorker <- Async.async $ do
                logInfo "Starting ogmios client"
                liftIO runOgmiosClient
                    `onException` ( do
                                        logWarning "Received exception while running ogmios client"
                                        void $ tryTakeMVar envOgmiosWorker
                                  )

            putSuccessful <- tryPutMVar envOgmiosWorker ogmiosWorker
            if putSuccessful
                then do
                    let resp = mkStartFetchBlocksResponse
                    sendTextData $ Json.encode resp
                else do
                    Async.cancel ogmiosWorker
                    logWarning "Another block fetcher was already running, cancelling worker thread"
                    let resp = mkStartFetchBlocksFault "Another block fetcher was already running, cancelling worker thread"
                    sendTextData $ Json.encode resp
  where
    sendTextData = liftIO . WS.sendTextData conn

cancelFetchBlocks :: WS.Connection -> App ()
cancelFetchBlocks conn = do
    Env{..} <- ask
    mogmiosWorker <- tryTakeMVar envOgmiosWorker
    case mogmiosWorker of
        Just ogmiosWorker -> do
            Async.cancel ogmiosWorker
            let resp = mkCancelFetchBlocksResponse
            sendTextData $ Json.encode resp
        Nothing -> do
            let resp = mkCancelFetchBlocksFault "No block fetcher running"
            sendTextData $ Json.encode resp
  where
    sendTextData = liftIO . WS.sendTextData conn

datumFilterAddHashes :: WS.Connection -> [Text] -> App ()
datumFilterAddHashes conn hashes = do
    Env{..} <- ask
    RequestedDatumHashes.add hashes envRequestedDatumHashes
    let resp = mkDatumFilterAddHashesResponse
    sendTextData $ Json.encode resp
  where
    sendTextData = liftIO . WS.sendTextData conn

datumFilterRemoveHashes :: WS.Connection -> [Text] -> App ()
datumFilterRemoveHashes conn hashes = do
    Env{..} <- ask
    RequestedDatumHashes.remove hashes envRequestedDatumHashes
    let resp = mkDatumFilterRemoveHashesResponse
    sendTextData $ Json.encode resp
  where
    sendTextData = liftIO . WS.sendTextData conn

datumFilterSetHashes :: WS.Connection -> [Text] -> App ()
datumFilterSetHashes conn hashes = do
    Env{..} <- ask
    RequestedDatumHashes.set hashes envRequestedDatumHashes
    let resp = mkDatumFilterSetHashesResponse
    sendTextData $ Json.encode resp
  where
    sendTextData = liftIO . WS.sendTextData conn

datumFilterGetHashes :: WS.Connection -> App ()
datumFilterGetHashes conn = do
    Env{..} <- ask
    hashSet <- RequestedDatumHashes.get envRequestedDatumHashes
    let resp = mkDatumFilterGetHashesResponse hashSet
    sendTextData $ Json.encode resp
  where
    sendTextData = liftIO . WS.sendTextData conn

websocketServer :: WS.Connection -> App ()
websocketServer conn = forever $ do
    jsonMsg <- receiveData
    case Json.decode @Method jsonMsg of
        Nothing -> do
            logError "Error parsing action"
        Just action ->
            case action of
                GetDatumByHash hash ->
                    getDatumByHash conn hash
                GetDatumsByHashes hashes ->
                    getDatumsByHashes conn hashes
                StartFetchBlocks firstBlockSlot firstBlockId ->
                    startFetchBlocks conn firstBlockSlot firstBlockId
                CancelFetchBlocks ->
                    cancelFetchBlocks conn
                DatumFilterAddHashes hashes ->
                    datumFilterAddHashes conn hashes
                DatumFilterRemoveHashes hashes ->
                    datumFilterRemoveHashes conn hashes
                DatumFilterSetHashes hashes ->
                    datumFilterSetHashes conn hashes
                DatumFilterGetHashes ->
                    datumFilterGetHashes conn
  where
    receiveData = liftIO $ WS.receiveData conn
