module Block.Fetch where

import Control.Exception (Exception)
import Control.Monad.Catch (throwM)
-- import Control.Concurrent (forkIO, threadDelay)
import UnliftIO.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, unless, forM_)
import Control.Monad.Trans (liftIO)
import qualified Network.WebSockets  as WS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Hasql.Connection as Hasql
import qualified Hasql.Session as Session
import qualified Data.Text.Encoding as Text
import qualified Data.Map as Map
import qualified Data.ByteString.Base64 as BSBase64
import qualified Data.Aeson as Json
import Control.Monad.Reader (ask)
import qualified Data.Set as Set

import qualified UnliftIO.Async as Async

import Block.Types
import Database
import App
import App.Env
import qualified App.RequestedDatumHashes as RequestedDatumHashes

receiveLoop :: WS.Connection -> App ()
receiveLoop conn = do
  jsonMsg <- liftIO $ WS.receiveData conn
  liftIO $ print jsonMsg

  msg <- maybe (throwM $ FindIntersectException "Can't decode response") pure $ Json.decode @OgmiosFindIntersectResponse jsonMsg
  liftIO $ print msg
  case _result msg of
    IntersectionNotFound _ -> do
      error "Intersection not found"
    IntersectionFound _ _ -> do
      Async.withAsync (receiveBlocksLoop conn) $ \receiveBlocksWorker -> do
        Async.link receiveBlocksWorker
        requestRemainingBlocks conn
        Async.wait receiveBlocksWorker

debounce :: App ()
debounce = threadDelay 100
-- debounce = threadDelay $ 10^6

requestRemainingBlocks :: WS.Connection -> App ()
requestRemainingBlocks conn = forever $ do
  liftIO $ WS.sendTextData conn (Json.encode $ mkRequestNextRequest 0)
  debounce

receiveBlocksLoop :: WS.Connection -> App ()
receiveBlocksLoop conn = forever $ do
  Env{..} <- ask
  let pgConn = envDbConnection
  requestedHashes <- RequestedDatumHashes.get envRequestedDatumHashes
  jsonMsg <- liftIO $ WS.receiveData conn
  msg <- maybe (throwM $ RequestNextException "Can't decode response") pure $ Json.decode @OgmiosRequestNextResponse jsonMsg

  case _result msg of
    RollBackward _point _tip -> do
      pure ()
    RollForward OtherBlock _tip -> do
      pure ()
    RollForward (MkAlonzoBlock block) _tip -> do

      let txDatums = map datums $ body block
      let filterDatums =
            if envSaveAllDatums
            then id
            else map (Map.filterWithKey (\k _ -> k `Set.member` requestedHashes))
      let requestedDatums = Map.unions (filterDatums txDatums)

      let decodeDatumValue = BSBase64.decodeBase64 . Text.encodeUtf8

      let (failedDecodings, requestedDatumsWithDecodedValues) = Map.mapEither decodeDatumValue requestedDatums
      unless (null failedDecodings) $ do
        liftIO $ Text.putStrLn $ "Error decoding values for datums: " <> (Text.intercalate ", " $ Map.keys failedDecodings)

      let savedHashes = Map.keys requestedDatums
      let savedValues = Map.elems requestedDatumsWithDecodedValues
      unless (null savedHashes) $ do
        liftIO $ Text.putStrLn $ "Inserting datums: " <> (Text.intercalate ", " savedHashes)
        res <- liftIO $ Session.run (insertDatumsSession savedHashes savedValues) envDbConnection
        liftIO $ print res

wsApp :: WS.Connection -> App ()
wsApp conn = do
    Env{..} <- ask
    liftIO $ putStrLn "Connected!"
    Async.withAsync (receiveLoop conn) $ \receiveWorker -> do
      Async.link receiveWorker
      let findIntersectRequest = mkFindIntersectRequest envFirstFetchBlock
      liftIO $ WS.sendTextData conn (Json.encode findIntersectRequest)
      debounce
      Async.wait receiveWorker
      liftIO $ WS.sendClose conn ("Fin" :: Text)

data FindIntersectException = FindIntersectException Text
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

data RequestNextException = RequestNextException Text
  deriving stock (Eq, Show)
  deriving anyclass (Exception)
