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
import Control.Concurrent.MVar (newMVar, newEmptyMVar)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.Catch (try, throwM, Exception)

import qualified Control.Concurrent.Async as Async

import qualified Colog as Colog
import Control.Monad.IO.Class (liftIO)

import qualified PlutusData
import Api (Routes, datumCacheApi)
import Api.Handler (datumServiceHandlers)
import Database (datumInsertSession, getDatumSession, Datum (..))
import Block.Fetch (wsApp)
import App.Env
import App
import Config
import App.FirstFetchBlock

appService :: Env App -> Application
appService env = serve datumCacheApi appServer
  where
    appServer :: ServerT (ToServantApi Routes) Handler
    appServer = hoistServer datumCacheApi hoistApp appServerT

    hoistApp :: App a -> Handler a
    hoistApp = Handler . ExceptT . try . flip runReaderT env . unApp

    appServerT :: ServerT (ToServantApi Routes) App
    appServerT = genericServerT datumServiceHandlers

data DbConnectionAcquireException = DbConnectionAcquireException Hasql.ConnectionError
  deriving stock (Eq, Show)
  deriving anyclass Exception

mkAppEnv :: Config -> IO (Env App)
mkAppEnv Config{..} = do
  pgConn <- Connection.acquire cfgDbConnectionString >>= either (throwM . DbConnectionAcquireException) pure
  requestedDatumHashes <- newMVar Set.empty
  let firstFetchBlock = FirstFetchBlock cfgFirstFetchBlockSlot cfgFirstFetchBlockId
  ogmiosWorker <- newEmptyMVar
  let env = Env requestedDatumHashes cfgSaveAllDatums firstFetchBlock pgConn Colog.richMessageAction cfgOgmiosAddress cfgOgmiosPort ogmiosWorker
  pure env

main :: IO ()
main = do
  cfg@Config{..} <- loadConfig
  -- CREATE TABLE datums (hash text, value bytea);
  -- CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS datums_hash_index ON datums (hash);
  env <- mkAppEnv cfg

  -- let runOgmiosClient = withSocketsDo $ WS.runClient cfgOgmiosAddress cfgOgmiosPort "" $
  --       (\wsConn -> runReaderT (unApp $ wsApp wsConn) env)

  -- Async.withAsync runOgmiosClient $ \ogmiosWorker -> do
  --   Async.link ogmiosWorker

  withStdoutLogger $ \logger -> do
    let warpSettings = W.setPort cfgServerPort $ W.setLogger logger W.defaultSettings
    W.runSettings warpSettings (appService env)
