module Block.Fetch (wsApp) where

import Colog (logError, logInfo, logWarning)
import Control.Exception (Exception)
import Control.Monad (forever, unless)
import Control.Monad.Reader (ask)
import Control.Monad.Trans (liftIO)
import Data.Aeson qualified as Json
import Data.ByteString.Base64 qualified as BSBase64
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Hasql.Session qualified as Session
import Network.WebSockets qualified as WS
import UnliftIO.Async qualified as Async
import UnliftIO.Concurrent (threadDelay)

import App (App)
import App.Env (Env (..))
import App.FirstFetchBlock
import App.RequestedDatumHashes qualified as RequestedDatumHashes
import Block.Types (AlonzoBlock (..), AlonzoTransaction (..), Block (..), FindIntersectResult (..), OgmiosFindIntersectResponse, OgmiosRequestNextResponse, OgmiosResponse (..), RequestNextResult (..), mkFindIntersectRequest, mkRequestNextRequest)
import Database (insertDatumsSession)

receiveLoop :: WS.Connection -> App ()
receiveLoop conn = do
    jsonMsg <- liftIO $ WS.receiveData conn
    let msg = Json.decode @OgmiosFindIntersectResponse jsonMsg
    case _result <$> msg of
        Nothing -> do
            logError "Error decoding FindIntersect response"
        Just (IntersectionNotFound _) -> do
            logError "Find intersection error: Intersection not found"
        -- throwM $ FindIntersectException "Intersection not found"
        Just (IntersectionFound _ _) -> do
            logInfo "Find intersection: intersection found, starting RequestNext loop"
            Async.withAsync (receiveBlocksLoop conn) $ \receiveBlocksWorker -> do
                Async.link receiveBlocksWorker
                requestRemainingBlocks conn
                Async.wait receiveBlocksWorker

debounce :: App ()
debounce = threadDelay 10

-- debounce = threadDelay $ 10 ^ 6

requestRemainingBlocks :: WS.Connection -> App ()
requestRemainingBlocks conn = forever $ do
    liftIO $ WS.sendTextData conn (Json.encode $ mkRequestNextRequest 0)
    debounce

receiveBlocksLoop :: WS.Connection -> App ()
receiveBlocksLoop conn = forever $ do
    Env{..} <- ask
    requestedHashes <- RequestedDatumHashes.get envRequestedDatumHashes
    jsonMsg <- liftIO $ WS.receiveData conn
    let msg = Json.decode @OgmiosRequestNextResponse jsonMsg

    case _result <$> msg of
        Nothing -> do
            logError "Error decoding RequestNext response"
            pure ()
        Just (RollBackward _point _tip) -> do
            logWarning "Received RollBackward response"
            pure ()
        Just (RollForward OtherBlock _tip) -> do
            logWarning "Received non-Alonzo block in the RollForward response"
            pure ()
        Just (RollForward (MkAlonzoBlock block) _tip) -> do
            let txDatums = map datums $ body block
            let filterDatums =
                    if envSaveAllDatums
                        then id
                        else map (Map.filterWithKey (\k _ -> k `Set.member` requestedHashes))
            let requestedDatums = Map.unions (filterDatums txDatums)

            let decodeDatumValue = BSBase64.decodeBase64 . Text.encodeUtf8

            let (failedDecodings, requestedDatumsWithDecodedValues) = Map.mapEither decodeDatumValue requestedDatums
            unless (null failedDecodings) $ do
                logError $ "Error decoding values for datums: " <> Text.intercalate ", " (Map.keys failedDecodings)

            let savedHashes = Map.keys requestedDatums
            let savedValues = Map.elems requestedDatumsWithDecodedValues
            unless (null savedHashes) $ do
                logInfo $ "Inserting datums: " <> Text.intercalate ", " savedHashes
                res <- liftIO $ Session.run (insertDatumsSession savedHashes savedValues) envDbConnection
                case res of
                    Right _ -> pure ()
                    Left err -> logError $ "Error inserting datums: " <> Text.pack (show err)

wsApp :: WS.Connection -> Maybe (Integer, Text) -> App ()
wsApp conn mfirstFetchBlock = do
    Env{..} <- ask
    logInfo "Connected to ogmios websocket"
    Async.withAsync (receiveLoop conn) $ \receiveWorker -> do
        Async.link receiveWorker
        let firstFetchBlock =
                case mfirstFetchBlock of
                    Just (firstBlockSlot, firstBlockId) ->
                        FirstFetchBlock firstBlockSlot firstBlockId
                    Nothing ->
                        envFirstFetchBlock
        let findIntersectRequest = mkFindIntersectRequest firstFetchBlock
        liftIO $ WS.sendTextData conn (Json.encode findIntersectRequest)
        debounce
        Async.wait receiveWorker
        liftIO $ WS.sendClose conn ("Fin" :: Text)

newtype FindIntersectException = FindIntersectException Text
    deriving stock (Eq, Show)
    deriving anyclass (Exception)
