module Api.WebSocket where

import qualified Network.WebSockets  as WS
import qualified Data.Aeson as Json
import Colog (logWarning, logInfo, logError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever)
import Data.Text (Text)
import Control.Monad.Reader (ask)

import qualified Hasql.Session as Session

import App
import App.Env
import Api.WebSocket.Json
import Api.WebSocket.Types
import qualified Database as Db

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
          let res = Json.Bool True
          datumRes <- liftIO (Session.run (Db.getDatumSession hash) envDbConnection)
          case datumRes of
            Left _ -> do
              let resp = mkGetDatumByHashResponse Nothing
              sendTextData $ Json.encode resp


          let sampleResp = mkGetDatumByHashResponse res
          sendTextData $ Json.encode sampleResp
  where
    receiveData = liftIO $ WS.receiveData conn
    sendTextData = liftIO . WS.sendTextData conn
