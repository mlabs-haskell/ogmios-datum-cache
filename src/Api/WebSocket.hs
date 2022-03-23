module Api.WebSocket (websocketServer) where

import Codec.Serialise qualified as Cbor
import Colog (logError, logInfo, logWarning)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, runReaderT)
import Data.Aeson (Value(Null))
import Data.Aeson qualified as Json
import Data.ByteString.Lazy qualified as BSL
import Data.Either (fromLeft, fromRight, isLeft)
import Data.HashMap.Strict qualified as HM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Hasql.Session qualified as Session
import Network.WebSockets qualified as WS
import UnliftIO.Async qualified as Async
import UnliftIO.Exception (onException)
import UnliftIO.MVar (isEmptyMVar, tryPutMVar, tryTakeMVar)

import Api.WebSocket.Json (
    JsonWspResponse,
    JsonWspFault,
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
    JsonWspRequest (..),
    Method(..)
 )
import App (App (..))
import App.Env (Env (..))
import App.RequestedDatumHashes qualified as RequestedDatumHashes
import Block.Fetch (wsApp)
import Database qualified as Db
import PlutusData qualified

toPlutusData :: Db.Datum -> Either Cbor.DeserialiseFailure PlutusData.Data
toPlutusData dt = Cbor.deserialiseOrFail @PlutusData.Data (BSL.fromStrict $ Db.value dt)

getDatumByHash :: Text -> App (Either JsonWspFault JsonWspResponse)
getDatumByHash hash = do
    Env{..} <- ask
    datumRes <- liftIO (Session.run (Db.getDatumSession hash) envDbConnection)
    case datumRes of
        Left _ -> do
            -- TODO: different response?
            pure $ Right $ mkGetDatumByHashResponse Nothing
        Right datum ->
            case toPlutusData datum of
                Left _ -> do
                    pure $ Left $ mkGetDatumByHashFault "Error deserializing plutus Data"
                Right plutusData -> do
                    let plutusDataJson = Json.toJSON plutusData
                    pure $ Right $ mkGetDatumByHashResponse (Just plutusDataJson)

getDatumsByHashes :: [Text] -> App (Either JsonWspFault JsonWspResponse)
getDatumsByHashes hashes = do
    Env{..} <- ask
    datumsRes <- liftIO (Session.run (Db.getDatumsSession hashes) envDbConnection)
    case datumsRes of
        Left _ -> do
            -- TODO: different response?
            pure $ Right $ mkGetDatumsByHashesResponse Nothing
        Right datums -> do
            let toPlutusDataOrErr :: Db.Datum -> Either Text GetDatumsByHashesDatum
                toPlutusDataOrErr dt =
                    case toPlutusData dt of
                        Left _ -> Left $ Db.hash dt
                        Right plutusData -> Right $ GetDatumsByHashesDatum (Db.hash dt) plutusData

            case Vector.partition isLeft $ Vector.map toPlutusDataOrErr datums of
                (linvalidDatums, rvalidDatums) | Vector.null linvalidDatums -> do
                    let plutusDataJson = Vector.toList $ Vector.map (fromRight Json.Null . (Json.toJSON <$>)) rvalidDatums
                    pure $ Right $ mkGetDatumsByHashesResponse (Just plutusDataJson)
                (linvalidDatums :: Vector.Vector (Either Text GetDatumsByHashesDatum), _) -> do
                    pure $ Left $ mkGetDatumsByHashesFault $ "Error deserializing plutus Data in: " <> Text.pack (show $ Vector.toList $ Vector.map (fromLeft "") linvalidDatums)

startFetchBlocks :: Integer -> Text -> App (Either JsonWspFault JsonWspResponse)
startFetchBlocks firstBlockSlot firstBlockId = do
    env@Env{..} <- ask
    isOgmiosWorkerRunning <- not <$> isEmptyMVar envOgmiosWorker
    if isOgmiosWorkerRunning
        then do
            pure $ Left $ mkStartFetchBlocksFault "Block fetcher already running"
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
                    pure $ Right mkStartFetchBlocksResponse
                else do
                    Async.cancel ogmiosWorker
                    logWarning "Another block fetcher was already running, cancelling worker thread"
                    pure $ Left $ mkStartFetchBlocksFault "Another block fetcher was already running, cancelling worker thread"

cancelFetchBlocks :: App (Either JsonWspFault JsonWspResponse)
cancelFetchBlocks = do
    Env{..} <- ask
    mogmiosWorker <- tryTakeMVar envOgmiosWorker
    case mogmiosWorker of
        Just ogmiosWorker -> do
            Async.cancel ogmiosWorker
            pure $ Right mkCancelFetchBlocksResponse
        Nothing ->
            pure $ Left $ mkCancelFetchBlocksFault "No block fetcher running"

datumFilterAddHashes :: [Text] -> App (Either JsonWspFault JsonWspResponse)
datumFilterAddHashes hashes = do
    Env{..} <- ask
    RequestedDatumHashes.add hashes envRequestedDatumHashes
    pure $ Right $ mkDatumFilterAddHashesResponse

datumFilterRemoveHashes :: [Text] -> App (Either JsonWspFault JsonWspResponse)
datumFilterRemoveHashes hashes = do
    Env{..} <- ask
    RequestedDatumHashes.remove hashes envRequestedDatumHashes
    pure $ Right $ mkDatumFilterRemoveHashesResponse

datumFilterSetHashes :: [Text] -> App (Either JsonWspFault JsonWspResponse)
datumFilterSetHashes hashes = do
    Env{..} <- ask
    RequestedDatumHashes.set hashes envRequestedDatumHashes
    pure $ Right $ mkDatumFilterSetHashesResponse

datumFilterGetHashes :: App (Either JsonWspFault JsonWspResponse)
datumFilterGetHashes = do
    Env{..} <- ask
    hashSet <- RequestedDatumHashes.get envRequestedDatumHashes
    pure $ Right $ mkDatumFilterGetHashesResponse hashSet

websocketServer :: WS.Connection -> App ()
websocketServer conn = forever $ do
    jsonMsg <- receiveData
    case Json.decode @JsonWspRequest jsonMsg of
        Nothing -> do
            logError "Error parsing action"
        Just (JsonWspRequest mirror method) -> do
            response <- case method of
                GetDatumByHash hash ->
                    getDatumByHash hash
                GetDatumsByHashes hashes ->
                    getDatumsByHashes hashes
                StartFetchBlocks firstBlockSlot firstBlockId ->
                    startFetchBlocks firstBlockSlot firstBlockId
                CancelFetchBlocks ->
                    cancelFetchBlocks
                DatumFilterAddHashes hashes ->
                    datumFilterAddHashes hashes
                DatumFilterRemoveHashes hashes ->
                    datumFilterRemoveHashes hashes
                DatumFilterSetHashes hashes ->
                    datumFilterSetHashes hashes
                DatumFilterGetHashes ->
                    datumFilterGetHashes

            let jsonResp = either Json.toJSON Json.toJSON response
            sendTextData $ appendJsonWspReflection (mirror, jsonResp)
  where
    sendTextData = liftIO . WS.sendTextData conn . Json.encode

    receiveData = liftIO $ WS.receiveData conn

    appendJsonWspReflection = \case
      (mirror, Json.Object jsonWspResponseObject) ->
        Json.Object $ HM.insert "reflection" (fromMaybe Null mirror) jsonWspResponseObject
      -- this should not be the case anyway
      (_, nonObject) -> nonObject
