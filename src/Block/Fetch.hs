module Block.Fetch (
  OgmiosWorkerMVar (MkOgmiosWorkerMVar),
  OgmiosInfo (..),
  ControlApiToken (..),
  StartBlockFetcherError (..),
  startBlockErrMsg,
  StopBlockFetcherError (..),
  stopBlockErrMsg,
  startBlockFetcher,
  stopBlockFetcher,
  createStoppedFetcher,
) where

import Control.Concurrent.MVar (
  MVar,
  newEmptyMVar,
  putMVar,
  takeMVar,
  tryPutMVar,
  tryTakeMVar,
 )
import Control.Exception (Exception, onException)
import Control.Monad (forever, unless, void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (
  MonadLogger,
  logErrorNS,
  logInfoNS,
  logWarnNS,
  runStdoutLoggingT,
 )
import Control.Monad.Reader qualified as Reader
import Control.Monad.Reader.Has (Has, MonadReader, ask, runReaderT)
import Control.Monad.Trans (liftIO)
import Data.Aeson qualified as Aeson
import Data.ByteString.Base64 qualified as Base64
import Data.Map qualified as Map
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Hasql.Connection qualified as Hasql
import Network.WebSockets qualified as WebSockets
import UnliftIO.Async (Async)
import UnliftIO.Async qualified as Async
import UnliftIO.Concurrent (threadDelay)

import Block.Filter (DatumFilter, runDatumFilter)
import Block.Types (
  AlonzoBlock (..),
  AlonzoBlockHeader (..),
  AlonzoTransaction (..),
  Block (..),
  BlockInfo (BlockInfo),
  FindIntersectResult (..),
  OgmiosFindIntersectResponse,
  OgmiosRequestNextResponse,
  OgmiosResponse (..),
  RequestNextResult (..),
  mkFindIntersectRequest,
  mkRequestNextRequest,
 )
import Database (saveDatums, updateLastBlock)

data OgmiosInfo = OgmiosInfo
  { ogmiosPort :: Int
  , ogmiosAddress :: String
  }

newtype OgmiosWorkerMVar = MkOgmiosWorkerMVar (MVar (Async ()))

newtype ControlApiToken = ControlApiToken (Maybe String)

withControlApiTokenToken ::
  (MonadReader r m, Has ControlApiToken r) =>
  e ->
  Maybe String ->
  m (Either e ()) ->
  m (Either e ())
withControlApiTokenToken err token action = do
  ControlApiToken expectToken <- ask
  case () of
    _
      | expectToken == Nothing -> action
      | expectToken == token -> action
      | otherwise -> pure $ Left err

data StartBlockFetcherError
  = StartBlockFetcherErrorAlreadyRunning
  | StartBlockNoControlApiTokenGranted
  deriving stock (Show)

startBlockErrMsg :: IsString s => StartBlockFetcherError -> s
startBlockErrMsg StartBlockFetcherErrorAlreadyRunning = "Block fetcher already running"
startBlockErrMsg StartBlockNoControlApiTokenGranted = "Control API token not granted"

startBlockFetcher ::
  ( MonadIO m
  , MonadUnliftIO m
  , MonadReader r m
  , Has OgmiosWorkerMVar r
  , Has OgmiosInfo r
  , Has Hasql.Connection r
  , Has ControlApiToken r
  ) =>
  BlockInfo ->
  DatumFilter ->
  Maybe String ->
  m (Either StartBlockFetcherError ())
startBlockFetcher blockInfo datumFilter token =
  withControlApiTokenToken StartBlockNoControlApiTokenGranted token $ do
    OgmiosInfo ogmiosPort ogmiosAddress <- ask
    MkOgmiosWorkerMVar envOgmiosWorker <- ask
    env <- Reader.ask
    canStart <- liftIO newEmptyMVar

    let runStack = runStdoutLoggingT . flip runReaderT env

    let errorHandler = runStack $ do
          logErrorNS "ogmiosWorker" "Error starting ogmios client"
          stopBlockFetcher token

    let runOgmiosClient = do
          takeMVar canStart
          WebSockets.runClient ogmiosAddress ogmiosPort "" $ \wsConn ->
            runStack $ Right <$> wsApp wsConn blockInfo datumFilter

    ogmiosWorker <- liftIO $
      Async.async $ do
        runStdoutLoggingT $ logInfoNS "ogmiosWorker" "Starting ogmios client"
        runOgmiosClient `onException` errorHandler

    putSuccessful <- liftIO $ tryPutMVar envOgmiosWorker $ void ogmiosWorker
    if putSuccessful
      then do
        liftIO $ putMVar canStart ()
        pure $ Right ()
      else do
        Async.cancel ogmiosWorker
        pure $ Left StartBlockFetcherErrorAlreadyRunning

data StopBlockFetcherError
  = StopBlockFetcherErrorNotRunning
  | StopBlockNoControlApiTokenGranted
  deriving stock (Show, Eq)

stopBlockErrMsg :: IsString s => StopBlockFetcherError -> s
stopBlockErrMsg StopBlockFetcherErrorNotRunning = "No block fetcher running"
stopBlockErrMsg StopBlockNoControlApiTokenGranted = "Control API token not granted"

stopBlockFetcher ::
  (MonadIO m, MonadReader r m, Has OgmiosWorkerMVar r, Has ControlApiToken r) =>
  Maybe String ->
  m (Either StopBlockFetcherError ())
stopBlockFetcher token =
  withControlApiTokenToken StopBlockNoControlApiTokenGranted token $ do
    MkOgmiosWorkerMVar envOgmiosWorker <- ask
    ogmiosWorker' <- liftIO $ tryTakeMVar envOgmiosWorker
    case ogmiosWorker' of
      Just ogmiosWorker -> do
        liftIO $ Async.cancel ogmiosWorker
        pure . pure $ ()
      Nothing -> pure $ Left StopBlockFetcherErrorNotRunning

createStoppedFetcher :: MonadIO m => m OgmiosWorkerMVar
createStoppedFetcher = MkOgmiosWorkerMVar <$> liftIO newEmptyMVar

receiveLoop ::
  ( MonadIO m
  , MonadUnliftIO m
  , MonadLogger m
  , MonadReader r m
  , Has Hasql.Connection r
  ) =>
  WebSockets.Connection ->
  DatumFilter ->
  m ()
receiveLoop conn datumFilter = do
  jsonMsg <- liftIO $ WebSockets.receiveData conn
  let msg = Aeson.decode @OgmiosFindIntersectResponse jsonMsg
  case _result <$> msg of
    Nothing -> do
      logErrorNS "receiveLoop" "Error decoding FindIntersect response"
    Just (IntersectionNotFound _) -> do
      logErrorNS
        "receiveLoop"
        "Find intersection error: Intersection not found. \
        \Consider restarting block fetcher with different block info"
    Just (IntersectionFound _ _) -> do
      logInfoNS
        "receiveLoop"
        "Find intersection: intersection found, starting RequestNext loop"
      Async.withAsync (receiveBlocksLoop conn datumFilter) $
        \receiveBlocksWorker -> do
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
  WebSockets.Connection ->
  m ()
requestRemainingBlocks conn = forever $ do
  liftIO $ WebSockets.sendTextData conn (Aeson.encode $ mkRequestNextRequest 0)
  debounce

receiveBlocksLoop ::
  (MonadIO m, MonadLogger m, MonadReader r m, Has Hasql.Connection r) =>
  WebSockets.Connection ->
  DatumFilter ->
  m ()
receiveBlocksLoop conn datumFilter = forever $ do
  jsonMsg <- liftIO $ WebSockets.receiveData conn
  let msg = Aeson.eitherDecode @OgmiosRequestNextResponse jsonMsg
  case _result <$> msg of
    Left e ->
      logErrorNS
        "receiveBlocksLoop"
        $ Text.pack $ "Error decoding RequestNext response: " <> e
    Right (RollBackward _point _tip) ->
      logWarnNS "receiveBlocksLoop" "Received RollBackward response"
    Right (RollForward OtherBlock _tip) ->
      logWarnNS
        "receiveBlocksLoop"
        "Received non-Alonzo block in the RollForward response"
    Right (RollForward (MkAlonzoBlock block) _tip) -> do
      logInfoNS "receiveBlocksLoop" $
        Text.pack $
          "Processing block: "
            <> show (slot $ header block, headerHash block)
      saveDatumsFromAlonzoBlock block datumFilter
      case headerHash block of
        Just headerHash' ->
          updateLastBlock $ BlockInfo (slot $ header block) headerHash'
        Nothing ->
          logWarnNS
            "receiveBlocksLoop"
            $ Text.pack $ "Block without header hash: " <> show block

saveDatumsFromAlonzoBlock ::
  (MonadIO m, MonadLogger m, MonadReader r m, Has Hasql.Connection r) =>
  AlonzoBlock ->
  DatumFilter ->
  m ()
saveDatumsFromAlonzoBlock block datumFilter = do
  let txs = body block
      getFilteredDatums tx =
        filter (runDatumFilter datumFilter tx) . Map.toList . datums $ tx
      requestedDatums =
        Map.fromList
          . concatMap getFilteredDatums
          $ txs
      decodeDatumValue = Base64.decodeBase64 . Text.encodeUtf8
      (failedDecodings, requestedDatumsWithDecodedValues) =
        Map.mapEither decodeDatumValue requestedDatums
  unless (null failedDecodings) $ do
    logErrorNS "saveDatumsFromAlonzoBlock" $
      "Error decoding values for datums: "
        <> Text.intercalate ", " (Map.keys failedDecodings)
    pure ()
  let datums = Map.toList requestedDatumsWithDecodedValues
  unless (null datums) $ saveDatums datums

wsApp ::
  ( MonadIO m
  , MonadUnliftIO m
  , MonadLogger m
  , MonadReader r m
  , Has Hasql.Connection r
  ) =>
  WebSockets.Connection ->
  BlockInfo ->
  DatumFilter ->
  m ()
wsApp conn blockInfo datumFilter = do
  logInfoNS "wsApp" "Connected to ogmios websocket"
  Async.withAsync (receiveLoop conn datumFilter) $ \receiveWorker -> do
    Async.link receiveWorker
    logInfoNS
      "wsApp"
      $ Text.pack $ "Starting fetcher from block: " <> show blockInfo
    let findIntersectRequest = mkFindIntersectRequest blockInfo
    liftIO $ WebSockets.sendTextData conn (Aeson.encode findIntersectRequest)
    debounce
    Async.wait receiveWorker
    liftIO $ WebSockets.sendClose conn ("Fin" :: Text)

newtype FindIntersectException = FindIntersectException Text
  deriving stock (Eq, Show)
  deriving anyclass (Exception)
