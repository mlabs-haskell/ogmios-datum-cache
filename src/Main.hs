{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Network.Socket (withSocketsDo)
import qualified Hasql.Connection as Connection
import qualified Hasql.Connection as Hasql
import qualified Network.WebSockets  as WS
import Network.Wai.Handler.Warp qualified as W
import Network.Wai.Logger (withStdoutLogger)
import Servant.Server.Generic   (genericServerT)
import Servant.Server (Application, Handler (..), serve, ServerT, hoistServer)
import Servant.API.Generic (ToServantApi)
import Control.Concurrent (forkIO)
import qualified Data.Set as Set
import Control.Concurrent.MVar (newMVar)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.Catch (try)

import qualified PlutusData
import Api (Routes, datumCacheApi)
import Api.Handler (datumServiceHandlers)
import Database (datumInsertSession, getDatumSession, Datum (..))
import Block.Fetch (wsApp)
import App.Env
import App

appService :: Env -> Application
appService env = serve datumCacheApi appServer
  where
    appServer :: ServerT (ToServantApi Routes) Handler
    appServer = hoistServer datumCacheApi hoistApp appServerT

    hoistApp :: App a -> Handler a
    hoistApp = Handler . ExceptT . try . flip runReaderT env . unApp

    appServerT :: ServerT (ToServantApi Routes) App
    appServerT = genericServerT datumServiceHandlers

main :: IO ()
main = do
  -- CREATE TABLE datums (hash text, value bytea);
  -- CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS datums_hash_index ON datums (hash);
  Right pgConn <- Connection.acquire connSettings

  -- Right datumRes <- Session.run (getDatumSession "ca3b51aec7831376f5cbbd96d3370f821b4593462edd4ea42e31edfd7fb03485") pgConn

  -- let Right sampleValue = BSBase64.decodeBase64 $ Text.encodeUtf8 "2GaCAIA="
  -- let sampleValue = value datumRes

  -- let Right plutusData = deserialiseOrFail @PlutusData.Data (BSL.fromStrict $ sampleValue)
  -- print plutusData
  -- print $ Json.encode plutusData

  requestedDatumHashes <- newMVar Set.empty
  let env = Env requestedDatumHashes True pgConn

  forkIO $ withSocketsDo $ WS.runClient "127.0.0.1" 1337 "" $
    (\wsConn -> runReaderT (unApp $ wsApp wsConn) env)

  let serverPort = 9999
  withStdoutLogger $ \logger -> do
    let warpSettings = W.setPort serverPort $ W.setLogger logger W.defaultSettings
    W.runSettings warpSettings (appService env)
  where
    connSettings = Connection.settings "localhost" 5432 "aske" "" "ogmios-datum-cache"
