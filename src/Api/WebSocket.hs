module Api.WebSocket where

import qualified Network.WebSockets  as WS
import qualified Data.Aeson as Json
import Colog (logWarning, logInfo, logError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever)
import Data.Text (Text)
import Control.Monad.Reader (ask)

import qualified Hasql.Session as Session
import qualified Data.ByteString.Lazy as BSL
import qualified Codec.Serialise as Cbor
import Data.Either (isLeft)
import qualified Data.Vector as Vector

import App
import App.Env
import Api.WebSocket.Json
import Api.WebSocket.Types
import qualified Database as Db
import qualified PlutusData

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
      case Vector.partition isLeft $ Vector.map (\dt -> case toPlutusData dt of
                                      Left _ -> Left $ Db.hash dt
                                      Right plutusData -> Right $ GetDatumsByHashesDatum (Db.hash dt) plutusData) datums of
        (invalidDatums, validDatums) | Vector.null invalidDatums -> do
          pure ()
        (invalidDatums, _) -> do
          pure ()
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
        GetDatumByHash hash -> getDatumByHash conn hash
        GetDatumsByHashes hashes -> getDatumsByHashes conn hashes
  where
    receiveData = liftIO $ WS.receiveData conn
