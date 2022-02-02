module Api.WebSocket where

import qualified Network.WebSockets  as WS
import qualified Data.Aeson as Json
import Colog (logWarning, logInfo, logError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever)
import Data.Text (Text)
import Control.Monad.Reader (ask)
import qualified Data.Text as Text
import qualified Hasql.Session as Session
import qualified Data.ByteString.Lazy as BSL
import qualified Codec.Serialise as Cbor
import Data.Either (isLeft, fromLeft, fromRight)
import qualified Data.Vector as Vector
import UnliftIO.MVar (tryTakeMVar, isEmptyMVar, tryPutMVar)
import qualified UnliftIO.Async as Async
import Control.Monad (void)
import UnliftIO.Exception (onException)
import Control.Monad.Reader (runReaderT)

import App
import App.Env
import Api.WebSocket.Json
import Api.WebSocket.Types
import qualified Database as Db
import qualified PlutusData
import Block.Fetch (wsApp)

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
      (liftIO runOgmiosClient) `onException` (do
        logWarning $ "Received exception while running ogmios client"
        void $ tryTakeMVar envOgmiosWorker)

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
  where
    receiveData = liftIO $ WS.receiveData conn
