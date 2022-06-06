module App (mkAppEnv, appService, DbConnectionAcquireException (..)) where

import Control.Monad.Catch (Exception, throwM, try)
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (runReaderT)
import Hasql.Connection qualified as Connection
import Hasql.Connection qualified as Hasql

import Api (Routes, datumCacheApi)
import Api.Handler (datumServiceHandlers)
import App.Env (
  Env (Env, envDbConnection, envOgmiosInfo, envOgmiosWorker),
 )
import App.Types (App (unApp))
import Block.Fetch (
  OgmiosInfo (OgmiosInfo),
  createStoppedFetcher,
 )
import Config (
  Config (
    Config,
    cfgDbConnectionString,
    cfgFetcher,
    cfgOgmiosAddress,
    cfgOgmiosPort,
    cfgServerPort
  ),
 )
import Servant.API.Generic (ToServantApi)
import Servant.Server (
  Application,
  Handler (Handler),
  ServerT,
  hoistServer,
  serve,
 )
import Servant.Server.Generic (genericServerT)

mkAppEnv :: Config -> IO Env
mkAppEnv Config {..} = do
  envDbConnection <-
    Connection.acquire cfgDbConnectionString
      >>= either (throwM . DbConnectionAcquireException) pure
  let envOgmiosInfo = OgmiosInfo cfgOgmiosPort cfgOgmiosAddress
  envOgmiosWorker <- createStoppedFetcher
  return Env {..}

newtype DbConnectionAcquireException
  = DbConnectionAcquireException Hasql.ConnectionError
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

appService :: Env -> Application
appService env = serve datumCacheApi appServer
  where
    appServer :: ServerT (ToServantApi Routes) Handler
    appServer = hoistServer datumCacheApi hoistApp appServerT

    hoistApp :: App a -> Handler a
    hoistApp = Handler . ExceptT . try . runStdoutLoggingT . flip runReaderT env . unApp

    appServerT :: ServerT (ToServantApi Routes) App
    appServerT = genericServerT datumServiceHandlers
