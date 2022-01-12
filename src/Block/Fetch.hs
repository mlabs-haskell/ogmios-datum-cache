module Block.Fetch where

import Control.Exception (throwIO, Exception)
import Control.Concurrent (forkIO, threadDelay)
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

import Block.Types
import Database
import App
import App.Env

-- TODO: ReaderT Env
receiveLoop :: WS.Connection -> Hasql.Connection -> IO ()
receiveLoop conn pgConn = do
  jsonMsg <- WS.receiveData conn
  print jsonMsg

  -- TODO: throwM
  msg <- maybe (throwIO $ FindIntersectException "Can't decode response") pure $ Json.decode @OgmiosFindIntersectResponse jsonMsg
  print msg
  case _result msg of
    IntersectionNotFound _ -> do
      error "Intersection not found"
    IntersectionFound _ _ -> do
      forkIO $ receiveBlocksLoop conn pgConn
      requestRemainingBlocks conn

requestRemainingBlocks :: WS.Connection -> IO ()
requestRemainingBlocks conn = forever $ do
  WS.sendTextData conn (Json.encode $ mkRequestNextRequest 0)
  threadDelay 100000

receiveBlocksLoop :: WS.Connection -> Hasql.Connection -> IO ()
receiveBlocksLoop conn pgConn = forever $ do
  jsonMsg <- WS.receiveData conn
  msg <- maybe (throwIO $ RequestNextException "Can't decode response") pure $ Json.decode @OgmiosRequestNextResponse jsonMsg

  case _result msg of
    RollBackward _point _tip -> do
      pure ()
    RollForward OtherBlock _tip -> do
      pure ()
    RollForward (MkAlonzoBlock block) _tip -> do
      -- TODO: multirow insert
      -- https://github.com/nikita-volkov/hasql/issues/25#issuecomment-286053459
      forM_ (body block) $ \tx -> do
        -- print $ datums tx
        forM_ (Map.toList $ datums tx) $ \(datumHash, datumValueBase64) -> do
          print (datumHash, datumValueBase64)
          -- print $ datumValueBase64
          -- print $ decodeBase64 datumValueBase64
          case BSBase64.decodeBase64 $ Text.encodeUtf8 datumValueBase64 of
            Left _ -> do
              Text.putStrLn $ "Error decoding value for " <> datumHash
            Right datumValue -> do
              Text.putStrLn $ "Inserting datum with sha256 hash: " <> datumHash
              res <- Session.run (datumInsertSession datumHash datumValue) pgConn
              -- TODO: err handling
              print res

  threadDelay 100000

wsApp :: WS.Connection -> App ()
wsApp conn = do
    Env{..} <- ask
    let pgConn = envDbConnection
    liftIO $ putStrLn "Connected!"
    liftIO $ forkIO $ receiveLoop conn pgConn

    liftIO $ WS.sendTextData conn findIntersect1
    liftIO $ threadDelay 10000000
    -- threadDelay 10000000000
    liftIO $ WS.sendClose conn ("Bye!" :: Text)

data FindIntersectException = FindIntersectException Text
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

data RequestNextException = RequestNextException Text
  deriving stock (Eq, Show)
  deriving anyclass (Exception)
