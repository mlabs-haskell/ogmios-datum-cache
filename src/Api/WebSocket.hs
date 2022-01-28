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
import Codec.Serialise (deserialiseOrFail)
import Data.Either (isLeft)

import App
import App.Env
import Api.WebSocket.Json
import Api.WebSocket.Types
import qualified Database as Db
import qualified PlutusData

websocketServer :: WS.Connection -> App ()
websocketServer conn = forever $ do
  Env{..} <- ask
  jsonMsg <- receiveData
  case Json.decode @Method jsonMsg of
    Nothing -> do
      logError "Error parsing action"

    Just action ->
      case action of
        GetDatumByHash hash -> do
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
        GetDatumsByHashes hashes -> do
          datumsRes <- liftIO (Session.run (Db.getDatumsSession hashes) envDbConnection)
          case datumsRes of
            Left _ -> do
              -- TODO: different response?
              let resp = mkGetDatumsByHashes Nothing
              sendTextData $ Json.encode resp
            Right datums -> do
              case partition isLeft $ map (\dt -> case toPlutusData dt of
                                              Left _ -> Left $ Db.hash dt
                                              Right plutusData -> Right $ GetDatumsByHashesDatum (Db.hash dt) plutusData) datums of
                ([], validDatums) -> do
                  pure ()
                (invalidDatums, _) -> do
                  pure ()


  where
    receiveData = liftIO $ WS.receiveData conn
    sendTextData = liftIO . WS.sendTextData conn
    toPlutusData dt = deserialiseOrFail @PlutusData.Data (BSL.fromStrict $ Db.value dt)
