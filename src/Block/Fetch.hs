module Block.Fetch (
  OgmiosInfo (..),
  BlockFetcherEnv,
  BlockProcessorEnv,
  startBlockFetcherAndProcessor,
  changeStartingBlock,
  changeDatumFilter,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (
  MVar,
  newEmptyMVar,
  newMVar,
  readMVar,
  swapMVar,
  tryPutMVar,
  withMVar,
 )
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO, readTBQueue, writeTBQueue)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Exception (SomeException, handle)
import Control.Monad (forever, guard, unless, void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (
  LogLevel (),
  LoggingT,
  MonadLogger,
  NoLoggingT (runNoLoggingT),
  filterLogger,
  logErrorNS,
  logInfoNS,
  logWarnNS,
  runStdoutLoggingT,
 )
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Reader.Has (Has)
import Control.Monad.Reader.Has qualified as Has
import Control.Monad.Trans (liftIO)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as ByteStringLazy
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.String (fromString)
import Data.String.ToString (toString)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Natural (Natural)
import Hasql.Connection qualified as Hasql
import Network.Socket qualified as Socket
import Network.WebSockets qualified as WebSockets
import Network.WebSockets.Stream qualified as Stream

import Block.Filter (DatumFilter, runDatumFilter)
import Block.Types (
  CursorPoint,
  FindIntersectResult (IntersectionFound, IntersectionNotFound),
  OgmiosFindIntersectResponse,
  OgmiosRequestNextResponse,
  OgmiosResponse (_result),
  RequestNextResult (RollBackward, RollForward),
  SomeBlock,
  StartingBlock (StartingBlock, Tip),
  blockToBlockInfo,
  blockTypeStr,
  datumsInTransaction,
  mkFindIntersectRequest,
  mkRequestNextRequest,
  rawTransactionsInBlock,
  tipToBlockInfo,
  transactionsInBlock,
 )
import Database (getLastBlock, saveDatums, saveTxs, updateLastBlock)

data OgmiosInfo = OgmiosInfo
  { ogmiosPort :: Int
  , ogmiosAddress :: String
  }

data BlockFetcherEnv = BlockFetcherEnv
  { -- | Queue used to push blocks for block processor.
    queue :: TBQueue SomeBlock
  , -- | WS connection to ogmios. Should be used only via 'sendAndReceive'.
    wsConnMVar :: MVar WebSockets.Connection
  , -- | Intersection from which block fetcher will start fetching blocks.
    -- Should be initialized only via `findIntersection`.
    intersectionTVar :: TVar (Maybe CursorPoint)
  }

data BlockProcessorEnv = BlockProcessorEnv
  { -- | Queue to read blocks from.
    queue :: TBQueue SomeBlock
  , -- | Datum filer.
    datumFilterMVar :: MVar DatumFilter
  , -- | Connection to postgres database.
    dbConn :: Hasql.Connection
  , -- | ogmios < 5.5 use base64, ogmios >= 5.5 use base16
    datumDecoder :: ByteString.ByteString -> Either Text ByteString.ByteString
  }

-- | Create new env for block processor.
mkBlockProcessorEnv ::
  MonadIO m =>
  DatumFilter ->
  Hasql.Connection ->
  Natural ->
  (ByteString.ByteString -> Either Text ByteString.ByteString) ->
  m BlockProcessorEnv
mkBlockProcessorEnv f c queueSize decoder = do
  q <- liftIO $ newTBQueueIO queueSize
  datumFilterMVar <- liftIO $ newMVar f
  pure $ BlockProcessorEnv q datumFilterMVar c decoder

-- | Create env for block fetcher connected to block processor's queue
mkBlockFetcherEnv ::
  MonadIO m =>
  BlockProcessorEnv ->
  WebSockets.Connection ->
  m BlockFetcherEnv
mkBlockFetcherEnv processorEnv wsConn = do
  wsConnMVar <- liftIO $ newMVar wsConn
  intersectionTVar <- liftIO $ newTVarIO Nothing
  pure $ BlockFetcherEnv processorEnv.queue wsConnMVar intersectionTVar

-- | like 'WebSockets.runClientWith' but without closing socket.
runClientWith' ::
  -- | Host
  String ->
  -- | Port
  Int ->
  -- | Path
  String ->
  -- | Options
  WebSockets.ConnectionOptions ->
  -- | Custom headers to send
  WebSockets.Headers ->
  -- | Client application
  WebSockets.ClientApp a ->
  IO a
runClientWith' host port path0 opts customHeaders app = do
  let hints =
        Socket.defaultHints
          { Socket.addrSocketType = Socket.Stream
          }
      fullHost = if port == 80 then host else host ++ ":" ++ show port
      path = if null path0 then "/" else path0
  addr : _ <- Socket.getAddrInfo (Just hints) (Just host) (Just $ show port)
  sock <- Socket.socket (Socket.addrFamily addr) Socket.Stream Socket.defaultProtocol
  Socket.setSocketOption sock Socket.NoDelay 1
  Socket.connect sock (Socket.addrAddress addr)
  stream <- Stream.makeSocketStream sock
  WebSockets.runClientWithStream stream fullHost path opts customHeaders app

-- | Start pair for workers fetching/processing blocks.
startBlockFetcherAndProcessor ::
  MonadIO m =>
  OgmiosInfo ->
  Hasql.Connection ->
  StartingBlock ->
  DatumFilter ->
  Natural ->
  (ByteString.ByteString -> Either Text ByteString.ByteString) ->
  (LogLevel -> Bool) ->
  m (MVar BlockFetcherEnv, BlockProcessorEnv)
startBlockFetcherAndProcessor
  ogmiosInfo
  dbConn
  blockInfo
  datumFilter
  queueSize
  decoder
  logFilter = do
    processorEnv <- mkBlockProcessorEnv datumFilter dbConn queueSize decoder
    liftIO
      . void
      . forkIO
      . flip runReaderT processorEnv
      . runLog
      . unBlockProcessorApp
      $ processLoop
    blockFetcherEnvMVar <- liftIO newEmptyMVar
    let handleException (e :: SomeException) = do
          runLog $
            logErrorNS
              "startBlockFetcherAndProcessor"
              $ "IO Exception occured, restarting block fetcher in 3s: " <> Text.pack (show e)
          -- TODO: do we want delay to be configurable?
          threadDelay 3_000_000
          lastBlock' <- runLog $ getLastBlock dbConn
          case lastBlock' of
            Nothing -> runInner blockInfo
            Just lastBlock -> runInner $ StartingBlock lastBlock
        runInner :: StartingBlock -> IO ()
        runInner startingBlock = handle handleException $
          runClientWith'
            ogmiosInfo.ogmiosAddress
            ogmiosInfo.ogmiosPort
            ""
            WebSockets.defaultConnectionOptions
            []
            $ \wsConn -> do
              fetcherEnv <- mkBlockFetcherEnv processorEnv wsConn
              -- Set MVar to new env, no matter if it's empty or not
              void $ tryPutMVar blockFetcherEnvMVar fetcherEnv
              void $ swapMVar blockFetcherEnvMVar fetcherEnv
              flip runReaderT fetcherEnv
                . runLog
                . unBlockFetcherApp
                $ fetchLoop startingBlock
    void . liftIO . forkIO . runInner $ blockInfo
    pure (blockFetcherEnvMVar, processorEnv)
    where
      runLog :: MonadIO m => LoggingT m a -> m a
      runLog = runStdoutLoggingT . filterLogger (const logFilter)

-- * Fetcher

newtype BlockFetcherApp a = BlockFetcherApp
  { unBlockFetcherApp :: LoggingT (ReaderT BlockFetcherEnv IO) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadReader BlockFetcherEnv)

-- | Indefinite loop fetching blocks from ogmios and pushing them to the queue.
fetchLoop ::
  (MonadIO m, MonadReader BlockFetcherEnv m, MonadLogger m) =>
  StartingBlock ->
  m ()
fetchLoop blockInfo = forever $ do
  logInfoNS "fetchLoop" "Starting..."
  void $ findIntersection blockInfo
  forever fetchNextBlock

-- | Function to interact with ogmios WS. Use *only* this function to use websocket.
sendAndReceive ::
  (MonadIO m, MonadReader BlockFetcherEnv m) =>
  ByteStringLazy.ByteString ->
  m ByteStringLazy.ByteString
sendAndReceive toSend = do
  env <- ask
  liftIO $
    withMVar env.wsConnMVar $ \wsConn -> do
      WebSockets.sendTextData wsConn toSend
      WebSockets.receiveData wsConn

-- | Wrapper for 'findIntersection' to be called from user-facing API.
changeStartingBlock ::
  (MonadIO m, MonadReader r m, Has (MVar BlockFetcherEnv) r) =>
  StartingBlock ->
  m (Maybe CursorPoint)
changeStartingBlock blockInfo = do
  env' <- Has.ask
  liftIO $
    withMVar env' $ \env ->
      flip runReaderT env . runNoLoggingT . findIntersection $ blockInfo

{- | Find intersection, unblock block fetcher to start fetching block starting from
 this intersection. If intersection is not found, but was initialized before block
 fetcher will continue running as before.
-}
findIntersection ::
  (MonadIO m, MonadLogger m, MonadReader BlockFetcherEnv m) =>
  StartingBlock ->
  m (Maybe CursorPoint)
findIntersection blockInfo = do
  env <- ask
  let setIntersection val = liftIO . atomically $ writeTVar env.intersectionTVar $ Just val
  let startFromTip tip = findIntersection $ StartingBlock $ tipToBlockInfo tip
  jsonMsg <- sendAndReceive $ Aeson.encode $ mkFindIntersectRequest blockInfo
  let msg = Aeson.decode @OgmiosFindIntersectResponse jsonMsg
  case _result <$> msg of
    Nothing -> do
      logErrorNS
        "findIntersection"
        "Error decoding WS response"
      pure Nothing
    Just (IntersectionNotFound tip) -> do
      case blockInfo of
        Tip -> startFromTip tip
        _ -> do
          logErrorNS
            "findIntersection"
            "Intersection not found. \
            \Consider restarting block fetcher with different block info"
          pure Nothing
    Just (IntersectionFound point tip) -> do
      case blockInfo of
        Tip -> startFromTip tip
        _ -> do
          logInfoNS
            "findIntersection"
            "Intersection found"
          setIntersection point
          pure $ Just point

{- | Request and get block from ogmios and push it to queue. Will block until
 intersection is set.
-}
fetchNextBlock ::
  (MonadIO m, MonadLogger m, MonadReader BlockFetcherEnv m) =>
  m ()
fetchNextBlock = do
  env <- ask

  -- Wait until intersection is found
  liftIO $
    atomically $
      readTVar env.intersectionTVar >>= guard . isJust

  jsonMsg <- sendAndReceive $ Aeson.encode $ mkRequestNextRequest 0
  let msg = Aeson.eitherDecode @OgmiosRequestNextResponse jsonMsg
  case _result <$> msg of
    Left e ->
      logErrorNS
        "fetchNextBlock"
        $ Text.pack $ "Error decoding RequestNext response: " <> e
    Right (RollBackward _point _tip) ->
      logWarnNS "fetchNextBlock" "Received RollBackward response"
    Right (RollForward block _tip) -> do
      liftIO $ atomically $ writeTBQueue env.queue block
      logInfoNS "fetchNextBlock" $
        "Fetched " <> blockTypeStr block <> " block: " <> Text.pack (show $ blockToBlockInfo block)

-- * Processor

newtype BlockProcessorApp a = BlockProcessorApp
  { unBlockProcessorApp :: LoggingT (ReaderT BlockProcessorEnv IO) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadReader BlockProcessorEnv)

-- | Indefinite loop, takes blocks from queue, extracts data and saves in db.
processLoop ::
  (MonadIO m, MonadReader BlockProcessorEnv m, MonadLogger m) =>
  m ()
processLoop = do
  logInfoNS "processLoop" "Starting..."
  env <- ask
  forever $ do
    -- TODO: maybe batching?
    block <- getBlock
    saveDatumsFromBlock block
    saveTxsFromBlock block
    updateLastBlock env.dbConn $ blockToBlockInfo block

-- | Pop block for queue, blocking if no block in queue.
getBlock ::
  (MonadIO m, MonadReader BlockProcessorEnv m) =>
  m SomeBlock
getBlock = do
  env <- ask
  liftIO $ atomically $ readTBQueue env.queue

-- | Extract and save datums from `DatumBlock` (alonzo and babbage).
saveDatumsFromBlock ::
  (MonadIO m, MonadReader BlockProcessorEnv m, MonadLogger m) =>
  SomeBlock ->
  m ()
saveDatumsFromBlock block = do
  env <- ask
  datumFilter <- liftIO $ readMVar env.datumFilterMVar
  let txs = transactionsInBlock block
      getFilteredDatums tx =
        filter (runDatumFilter datumFilter tx) . Map.toList . datumsInTransaction $ tx
      requestedDatums =
        Map.fromList
          . concatMap getFilteredDatums
          $ txs
      decodeDatumValue dt = env.datumDecoder $ fromString $ toString dt
      (failedDecodings, requestedDatumsWithDecodedValues) =
        Map.mapEither decodeDatumValue requestedDatums
  unless (null failedDecodings) $ do
    logErrorNS "saveDatumsFromBlock" $
      "Error decoding values for datums (Base64 or Base16): "
        <> Text.intercalate ", " (Map.keys failedDecodings)
    pure ()
  let datums_ = Map.toList requestedDatumsWithDecodedValues
  unless (null datums_) $ saveDatums env.dbConn datums_

saveTxsFromBlock ::
  (MonadIO m, MonadReader BlockProcessorEnv m, MonadLogger m) =>
  SomeBlock ->
  m ()
saveTxsFromBlock block = do
  env <- ask
  let txs = rawTransactionsInBlock block
  unless (null txs) $ saveTxs env.dbConn txs

-- | Change block processor's datum filer. Safe to call from user facing API.
changeDatumFilter ::
  (MonadIO m, MonadReader r m, Has BlockProcessorEnv r) =>
  DatumFilter ->
  m ()
changeDatumFilter datumFilter = do
  env :: BlockProcessorEnv <- Has.ask
  void $ liftIO $ swapMVar env.datumFilterMVar datumFilter
