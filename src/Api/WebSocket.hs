module Api.WebSocket where

import qualified Network.WebSockets  as WS
import qualified Data.Aeson as Json
import Colog (logWarning, logInfo, logError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever)
import Data.Text (Text)

import App
import Api.WebSocket.Types

websocketServer :: WS.Connection -> App ()
websocketServer conn = forever $ do
  jsonMsg <- receiveData
  case Json.decode @Method jsonMsg of
    Nothing -> do
      logError "Error parsing action"

    Just action ->
      case action of
        GetDatumByHash hash -> do
          sendTextData ("{}" :: Text)
  where
    receiveData = liftIO $ WS.receiveData conn
    sendTextData = liftIO . WS.sendTextData conn
