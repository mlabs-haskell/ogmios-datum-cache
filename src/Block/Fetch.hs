module Block.Fetch (
  OgmiosInfo (..),
  startBlockFetcherAndProcessor,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (
  MVar,
  newMVar,
  readMVar,
  withMVar,
 )
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO, readTBQueue, writeTBQueue)
import Control.Exception (SomeException, handle)
import Control.Monad (forever, unless, void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (
  LoggingT,
  MonadLogger,
  logErrorNS,
  logInfoNS,
  logWarnNS,
  runStdoutLoggingT,
 )
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans (liftIO)
import Data.Aeson qualified as Aeson
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy (ByteString)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Hasql.Connection qualified as Hasql
import Network.Socket qualified as S
import Network.WebSockets qualified as WebSockets
import Network.WebSockets.Stream qualified as Stream

import Block.Filter (DatumFilter, runDatumFilter)
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
import Database (getLastBlock, saveDatums, updateLastBlock)

data OgmiosInfo = OgmiosInfo
  { ogmiosPort :: Int
  , ogmiosAddress :: String
  }

data BlockFetcherEnv = BlockFetcherEnv
  { queue :: TBQueue AlonzoBlock
  , wsConnMVar :: MVar WebSockets.Connection
  }

data BlockProcessorEnv = BlockProcessorEnv
  { queue :: TBQueue AlonzoBlock
  , datumFilterMVar :: MVar DatumFilter
  , dbConn :: Hasql.Connection
  }

mkBlockProcessorEnv ::
  MonadIO m =>
  DatumFilter ->
  Hasql.Connection ->
  m BlockProcessorEnv
mkBlockProcessorEnv f c = do
  -- TODO: make it configurable
  q <- liftIO $ newTBQueueIO 64
  datumFilterMVar <- liftIO $ newMVar f
  pure $ BlockProcessorEnv q datumFilterMVar c

mkBlockFetcherEnv :: MonadIO m => BlockProcessorEnv -> WebSockets.Connection -> m BlockFetcherEnv
mkBlockFetcherEnv processorEnv wsConn = do
  wsConnMVar <- liftIO $ newMVar wsConn
  pure $ BlockFetcherEnv processorEnv.queue wsConnMVar

-- | like 'WebSockets.runClientWith' but without closing socket
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
        S.defaultHints
          { S.addrSocketType = S.Stream
          }
      fullHost = if port == 80 then host else host ++ ":" ++ show port
      path = if null path0 then "/" else path0
  addr : _ <- S.getAddrInfo (Just hints) (Just host) (Just $ show port)
  sock <- S.socket (S.addrFamily addr) S.Stream S.defaultProtocol
  S.setSocketOption sock S.NoDelay 1
  S.connect sock (S.addrAddress addr)
  stream <- Stream.makeSocketStream sock
  WebSockets.runClientWithStream stream fullHost path opts customHeaders app

startBlockFetcherAndProcessor ::
  MonadIO m =>
  OgmiosInfo ->
  Hasql.Connection ->
  BlockInfo ->
  DatumFilter ->
  m ()
startBlockFetcherAndProcessor (OgmiosInfo ogmiosPort ogmiosAddress) dbConn blockInfo datumFilter = do
  processorEnv <- mkBlockProcessorEnv datumFilter dbConn
  liftIO
    . void
    . forkIO
    . flip runReaderT processorEnv
    . runStdoutLoggingT
    . unBlockProcessorApp
    $ processLoop

  let handleExcpetion (e :: SomeException) = do
        -- TODO: do we want delay to be configurable?
        putStr "IO Exception occured, restarting block fetcher in 3s: "
        print e
        threadDelay 3_000_000
        lastBlock' <- runStdoutLoggingT $ getLastBlock dbConn
        case lastBlock' of
          Nothing -> runInner blockInfo
          Just lastBlock -> runInner lastBlock
      runInner startingBlock = handle handleExcpetion $
        runClientWith' ogmiosAddress ogmiosPort "" WebSockets.defaultConnectionOptions [] $ \wsConn -> do
          fetcherEnv <- mkBlockFetcherEnv processorEnv wsConn
          flip runReaderT fetcherEnv
            . runStdoutLoggingT
            . unBlockFetcherApp
            $ fetchLoop startingBlock
  void . liftIO . forkIO . runInner $ blockInfo

------ Fetcher

newtype BlockFetcherApp a = BlockFetcherApp
  { unBlockFetcherApp :: LoggingT (ReaderT BlockFetcherEnv IO) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadReader BlockFetcherEnv)

fetchLoop :: BlockInfo -> BlockFetcherApp ()
fetchLoop blockInfo = forever $ do
  logInfoNS "fetchLoop" "Starting..."
  findIntersection blockInfo
  forever receiveBlock

sendAndReceive :: ByteString -> BlockFetcherApp ByteString
sendAndReceive toSend = do
  env <- ask
  liftIO $
    withMVar env.wsConnMVar $ \wsConn -> do
      WebSockets.sendTextData wsConn toSend
      WebSockets.receiveData wsConn

findIntersection :: BlockInfo -> BlockFetcherApp ()
findIntersection blockInfo = do
  jsonMsg <- sendAndReceive $ Aeson.encode $ mkFindIntersectRequest blockInfo
  let msg = Aeson.decode @OgmiosFindIntersectResponse jsonMsg
  case _result <$> msg of
    Nothing -> do
      logErrorNS
        "findIntersection"
        "Error decoding WS response"
    Just (IntersectionNotFound _) -> do
      logErrorNS
        "findIntersection"
        "Intersection not found. \
        \Consider restarting block fetcher with different block info"
    Just (IntersectionFound _ _) -> do
      logInfoNS
        "findIntersection"
        "Intersection found"

receiveBlock :: BlockFetcherApp ()
receiveBlock = do
  env <- ask
  jsonMsg <- sendAndReceive $ Aeson.encode $ mkRequestNextRequest 0
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
      liftIO $ atomically $ writeTBQueue env.queue block
      logInfoNS "receiveBlocksLoop" $
        Text.pack $
          "Fetched block: "
            <> show (slot $ header block, headerHash block)

----- Processor

newtype BlockProcessorApp a = BlockProcessorApp
  { unBlockProcessorApp :: LoggingT (ReaderT BlockProcessorEnv IO) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadReader BlockProcessorEnv)

processLoop :: BlockProcessorApp ()
processLoop = do
  logInfoNS "processLoop" "Starting..."
  env <- ask
  forever $ do
    -- TODO: maybe batching?
    block <- getBlock
    saveDatumsFromAlonzoBlock block
    updateLastBlock env.dbConn (BlockInfo block.header.slot block.headerHash)

getBlock :: BlockProcessorApp AlonzoBlock
getBlock = do
  env <- ask
  liftIO $ atomically $ readTBQueue env.queue

saveDatumsFromAlonzoBlock :: AlonzoBlock -> BlockProcessorApp ()
saveDatumsFromAlonzoBlock block = do
  env <- ask
  datumFilter <- liftIO $ readMVar env.datumFilterMVar
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
  unless (null datums) $ saveDatums env.dbConn datums
