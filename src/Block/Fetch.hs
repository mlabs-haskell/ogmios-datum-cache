module Block.Fetch (
    OgmiosWorkerMVar (MkOgmiosWorkerMVar),
    OgmiosInfo (..),
    StartBlockFetcherError (..),
    StopBlockFetcherError (..),
    startBlockFetcher,
    stopBlockFetcher,
    isBlockFetcherRunning,
    createStoppedFetcher,
) where

import Control.Concurrent.MVar (MVar, isEmptyMVar, newEmptyMVar, tryPutMVar, tryTakeMVar)
import Control.Exception (Exception, onException)
import Control.Monad (forever, unless, void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger, logErrorNS, logInfoNS, logWarnNS, runStdoutLoggingT)
import Control.Monad.Reader qualified as Reader
import Control.Monad.Reader.Has (Has, MonadReader, ask, runReaderT)
import Control.Monad.Trans (liftIO)
import Data.Aeson qualified as Json
import Data.ByteString.Base64 qualified as BSBase64
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Hasql.Connection qualified as Hasql
import Hasql.Session qualified as Session
import Network.WebSockets qualified as WS
import UnliftIO.Async (Async)
import UnliftIO.Async qualified as Async
import UnliftIO.Concurrent (threadDelay)

import Api.Types (FirstFetchBlock (FirstFetchBlock))
import Block.Filter (DatumFilter, runDatumFilter)
import Block.Types (AlonzoBlock (..), AlonzoTransaction (..), Block (..), FindIntersectResult (..), OgmiosFindIntersectResponse, OgmiosRequestNextResponse, OgmiosResponse (..), RequestNextResult (..), mkFindIntersectRequest, mkRequestNextRequest)
import Database (insertDatumsSession)

data OgmiosInfo = OgmiosInfo
    { ogmiosPort :: Int
    , ogmiosAddress :: String
    }

newtype OgmiosWorkerMVar = MkOgmiosWorkerMVar (MVar (Async ()))

data StartBlockFetcherError
    = StartBlockFetcherErrorAlreadyRunning

startBlockFetcher ::
    (MonadIO m, MonadUnliftIO m, MonadReader r m, Has OgmiosWorkerMVar r, Has OgmiosInfo r, Has FirstFetchBlock r, Has DatumFilter r, Has Hasql.Connection r) =>
    Maybe (Integer, Text) ->
    m (Either StartBlockFetcherError ())
startBlockFetcher firstBlock = do
    OgmiosInfo{..} <- ask
    (MkOgmiosWorkerMVar envOgmiosWorker) <- ask
    env <- Reader.ask

    let runStack = runStdoutLoggingT . flip runReaderT env

    let errorHandler = runStack $ do
            logErrorNS "ogmiosWorker" "Error starting ogmios client"
            stopBlockFetcher

    let runOgmiosClient =
            WS.runClient ogmiosAddress ogmiosPort "" $ \wsConn ->
                runStack $ fmap Right $ wsApp wsConn firstBlock

    ogmiosWorker <- liftIO $
        Async.async $ do
            runStdoutLoggingT $ logInfoNS "ogmiosWorker" "Starting ogmios client"
            runOgmiosClient `onException` errorHandler

    putSuccessful <- liftIO $ tryPutMVar envOgmiosWorker $ void ogmiosWorker

    case putSuccessful of
        True -> pure . pure $ ()
        False -> pure $ Left StartBlockFetcherErrorAlreadyRunning

isBlockFetcherRunning :: (MonadIO m) => OgmiosWorkerMVar -> m Bool
isBlockFetcherRunning (MkOgmiosWorkerMVar mvar) = liftIO $ isEmptyMVar mvar

data StopBlockFetcherError
    = StopBlockFetcherErrorNotRunning
    deriving stock (Show, Eq)

stopBlockFetcher :: (MonadIO m, MonadReader r m, Has OgmiosWorkerMVar r) => m (Either StopBlockFetcherError ())
stopBlockFetcher = do
    (MkOgmiosWorkerMVar envOgmiosWorker) <- ask
    ogmiosWorker' <- liftIO $ tryTakeMVar envOgmiosWorker
    case ogmiosWorker' of
        Just ogmiosWorker -> do
            liftIO $ Async.cancel ogmiosWorker
            pure . pure $ ()
        Nothing -> pure $ Left StopBlockFetcherErrorNotRunning

createStoppedFetcher :: MonadIO m => m OgmiosWorkerMVar
createStoppedFetcher = MkOgmiosWorkerMVar <$> liftIO newEmptyMVar

receiveLoop ::
    (MonadIO m, MonadUnliftIO m, MonadLogger m, MonadReader r m, Has DatumFilter r, Has Hasql.Connection r) =>
    WS.Connection ->
    m ()
receiveLoop conn = do
    jsonMsg <- liftIO $ WS.receiveData conn
    let msg = Json.decode @OgmiosFindIntersectResponse jsonMsg
    case _result <$> msg of
        Nothing -> do
            logErrorNS "receiveLoop" "Error decoding FindIntersect response"
            pure ()
        Just (IntersectionNotFound _) -> do
            logErrorNS "receiveLoop" "Find intersection error: Intersection not found"
            pure ()
        Just (IntersectionFound _ _) -> do
            logInfoNS "receiveLoop" "Find intersection: intersection found, starting RequestNext loop"
            Async.withAsync (receiveBlocksLoop conn) $ \receiveBlocksWorker -> do
                Async.link receiveBlocksWorker
                requestRemainingBlocks conn
                Async.wait receiveBlocksWorker

-- Why it's neccesary?
debounce ::
    MonadIO m =>
    m ()
debounce = liftIO $ threadDelay 10

requestRemainingBlocks ::
    MonadIO m =>
    WS.Connection ->
    m ()
requestRemainingBlocks conn = forever $ do
    liftIO $ WS.sendTextData conn (Json.encode $ mkRequestNextRequest 0)
    debounce

receiveBlocksLoop ::
    (MonadIO m, MonadLogger m, MonadReader r m, Has DatumFilter r, Has Hasql.Connection r) =>
    WS.Connection ->
    m ()
receiveBlocksLoop conn = forever $ do
    jsonMsg <- liftIO $ WS.receiveData conn
    let msg = Json.eitherDecode @OgmiosRequestNextResponse jsonMsg
    case _result <$> msg of
        Left e ->
            logErrorNS "receiveBlocksLoop" $ Text.pack $ "Error decoding RequestNext response: " <> e
        Right (RollBackward _point _tip) ->
            logWarnNS "receiveBlocksLoop" "Received RollBackward response"
        Right (RollForward OtherBlock _tip) ->
            logWarnNS "receiveBlocksLoop" "Received non-Alonzo block in the RollForward response"
        Right (RollForward (MkAlonzoBlock block) _tip) -> do
            logInfoNS "receiveBlocksLoop" $ Text.pack $ "Processing block: " <> show (header block)
            saveDatumsFromAlonzoBlock block

saveDatumsFromAlonzoBlock ::
    (MonadIO m, MonadLogger m, MonadReader r m, Has DatumFilter r, Has Hasql.Connection r) =>
    AlonzoBlock ->
    m ()
saveDatumsFromAlonzoBlock block = do
    datumFilter <- ask
    dbConnection <- ask
    let txs = body block
    let requestedDatums =
            Map.fromList
                . concatMap (\tx -> (filter (runDatumFilter datumFilter tx) . Map.toList . datums) tx)
                $ txs
    let decodeDatumValue = BSBase64.decodeBase64 . Text.encodeUtf8
    let (failedDecodings, requestedDatumsWithDecodedValues) = Map.mapEither decodeDatumValue requestedDatums
    unless (null failedDecodings) $ do
        logErrorNS "saveDatumsFromAlonzoBlock" $ "Error decoding values for datums: " <> Text.intercalate ", " (Map.keys failedDecodings)
        pure ()
    let savedHashes = Map.keys requestedDatums
    let savedValues = Map.elems requestedDatumsWithDecodedValues
    unless (null savedHashes) $ do
        logInfoNS "saveDatumsFromAlonzoBlock" $ "Inserting datums: " <> Text.intercalate ", " savedHashes
        res <- liftIO $ Session.run (insertDatumsSession savedHashes savedValues) dbConnection
        case res of
            Right _ -> pure ()
            Left err -> do
                logErrorNS "saveDatumsFromAlonzoBlock" $ "Error inserting datums: " <> Text.pack (show err)
                pure ()

wsApp ::
    (MonadIO m, MonadUnliftIO m, MonadLogger m, MonadReader r m, Has FirstFetchBlock r, Has DatumFilter r, Has Hasql.Connection r) =>
    WS.Connection ->
    Maybe (Integer, Text) ->
    m ()
wsApp conn mfirstFetchBlock = do
    firstFetchBlock' :: FirstFetchBlock <- ask
    logInfoNS "wsApp" "Connected to ogmios websocket"
    Async.withAsync (receiveLoop conn) $ \receiveWorker -> do
        Async.link receiveWorker
        let firstFetchBlock =
                case mfirstFetchBlock of
                    Just (firstBlockSlot, firstBlockId) ->
                        FirstFetchBlock firstBlockSlot firstBlockId
                    Nothing ->
                        firstFetchBlock'
        let findIntersectRequest = mkFindIntersectRequest firstFetchBlock
        liftIO $ WS.sendTextData conn (Json.encode findIntersectRequest)
        debounce
        Async.wait receiveWorker
        liftIO $ WS.sendClose conn ("Fin" :: Text)

newtype FindIntersectException = FindIntersectException Text
    deriving stock (Eq, Show)
    deriving anyclass (Exception)
