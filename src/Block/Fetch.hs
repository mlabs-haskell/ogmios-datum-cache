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

import Block.Types
import Database
import App
import App.Env
import qualified App.RequestedDatumHashes as RequestedDatumHashes

-- TODO: UnliftIO.Async async
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
      forkIO $ receiveBlocksLoop conn
      requestRemainingBlocks conn

requestRemainingBlocks :: WS.Connection -> App ()
requestRemainingBlocks conn = forever $ do
  liftIO $ WS.sendTextData conn (Json.encode $ mkRequestNextRequest 0)
  threadDelay 100000

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
        res <- liftIO $ Session.run (insertDatumsSession savedHashes savedValues) pgConn
        liftIO $ print res

  threadDelay 100000

wsApp :: WS.Connection -> App ()
wsApp conn = do
    Env{..} <- ask
    let pgConn = envDbConnection
    liftIO $ putStrLn "Connected!"
    forkIO $ receiveLoop conn

    let findIntersectRequest = mkFindIntersectRequest envFirstFetchBlock
    liftIO $ WS.sendTextData conn (Json.encode findIntersectRequest)
    threadDelay 10000000
    liftIO $ WS.sendClose conn ("Bye!" :: Text)

data FindIntersectException = FindIntersectException Text
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

data RequestNextException = RequestNextException Text
  deriving stock (Eq, Show)
  deriving anyclass (Exception)
