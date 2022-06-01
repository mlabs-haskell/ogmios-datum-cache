module Block.Fetch (
  OgmiosWorkerMVar (MkOgmiosWorkerMVar),
  OgmiosInfo (..),
  StartBlockFetcherError (..),
  StopBlockFetcherError (..),
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
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Hasql.Connection qualified as Hasql
import Network.WebSockets qualified as WebSockets
import UnliftIO.Async (Async)
import UnliftIO.Async qualified as Async
import UnliftIO.Concurrent (threadDelay)

import Block.Filter (DatumFilter, runDatumFilter, TxFilter, runFilter)
import Block.Types (
  AlonzoBlock (body, header, headerHash),
  AlonzoBlockHeader (slot),
  AlonzoTransaction (datums),
  Block (MkAlonzoBlock, OtherBlock),
  BlockInfo (BlockInfo),
  FindIntersectResult (IntersectionFound, IntersectionNotFound),
  OgmiosFindIntersectResponse,
  OgmiosRequestNextResponse,
  OgmiosResponse (_result),
  RequestNextResult (RollBackward, RollForward),
  mkFindIntersectRequest,
  mkRequestNextRequest,
 )
import Database (saveTransactions, updateLastBlock)

data OgmiosInfo = OgmiosInfo
  { ogmiosPort :: Int
  , ogmiosAddress :: String
  }

newtype OgmiosWorkerMVar = MkOgmiosWorkerMVar (MVar (Async ()))

data StartBlockFetcherError
  = StartBlockFetcherErrorAlreadyRunning
  deriving stock (Show)

startBlockFetcher ::
  ( MonadIO m
  , MonadUnliftIO m
  , MonadReader r m
  , Has OgmiosWorkerMVar r
  , Has OgmiosInfo r
  , Has Hasql.Connection r
  ) =>
  BlockInfo ->
  TxFilter ->
  m (Either StartBlockFetcherError ())
startBlockFetcher blockInfo txFilter = do
  OgmiosInfo ogmiosPort ogmiosAddress <- ask
  MkOgmiosWorkerMVar envOgmiosWorker <- ask
  env <- Reader.ask
  canStart <- liftIO newEmptyMVar

  let runStack = runStdoutLoggingT . flip runReaderT env

  let errorHandler = runStack $ do
        logErrorNS "ogmiosWorker" "Error starting ogmios client"
        stopBlockFetcher

  let runOgmiosClient = do
        takeMVar canStart
        WebSockets.runClient ogmiosAddress ogmiosPort "" $ \wsConn ->
          runStack $ Right <$> wsApp wsConn blockInfo txFilter

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
  deriving stock (Show, Eq)

stopBlockFetcher ::
  (MonadIO m, MonadReader r m, Has OgmiosWorkerMVar r) =>
  m (Either StopBlockFetcherError ())
stopBlockFetcher = do
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
  TxFilter ->
  m ()
receiveLoop conn txFilter = do
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
      Async.withAsync (receiveBlocksLoop conn txFilter) $
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
  TxFilter ->
  m ()
receiveBlocksLoop conn txFilter = forever $ do
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
      saveTransactionsFromAlonzoBlock block txFilter
      case headerHash block of
        Just headerHash' ->
          updateLastBlock $ BlockInfo (slot $ header block) headerHash'
        Nothing ->
          logWarnNS
            "receiveBlocksLoop"
            $ Text.pack $ "Block without header hash: " <> show block

saveTransactionsFromAlonzoBlock ::
  (MonadIO m, MonadLogger m, MonadReader r m, Has Hasql.Connection r) =>
  AlonzoBlock ->
  TxFilter ->
  m ()
saveTransactionsFromAlonzoBlock block txFilter = do
  let txs = body block
      requestedTransactions = filter (runFilter txFilter) txs
      {-decodeDatumValue = Base64.decodeBase64 . Text.encodeUtf8
      (failedDecodings, requestedDatumsWithDecodedValues) =
        Map.mapEither decodeDatumValue requestedDatums
  unless (null failedDecodings) $ do
    logErrorNS "saveDatumsFromAlonzoBlock" $
      "Error decoding values for datums: "
        <> Text.intercalate ", " (Map.keys failedDecodings)
    pure ()
  -- let datums = Map.toList requestedDatumsWithDecodedValues-}
  let rTx = requestedTransactions
  unless (null rTx) $ saveTransactions rTx

wsApp ::
  ( MonadIO m
  , MonadUnliftIO m
  , MonadLogger m
  , MonadReader r m
  , Has Hasql.Connection r
  ) =>
  WebSockets.Connection ->
  BlockInfo ->
  TxFilter ->
  m ()
wsApp conn blockInfo txFilter = do
  logInfoNS "wsApp" "Connected to ogmios websocket"
  Async.withAsync (receiveLoop conn txFilter) $ \receiveWorker -> do
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
