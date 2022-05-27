module App (mkAppEnv, appService, DbConnectionAcquireException (..)) where

import Control.Monad.Catch (Exception, throwM, try)
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (runReaderT)
import Hasql.Connection qualified as Connection
import Hasql.Connection qualified as Hasql

import Api (Routes, datumCacheApi, datumCacheContext)
import Api.Handler (controlApiAuthCheck, datumServiceHandlers)
import Api.Types (ControlApiAuthData)
import App.Env (
  ControlApiToken (ControlApiToken),
  Env (Env, envControlApiToken, envDbConnection, envOgmiosInfo, envOgmiosWorker),
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
    cfgServerControlApiToken,
    cfgServerPort
  ),
 )
import Servant.API.Generic (ToServantApi)
import Servant.Server (
  Application,
  BasicAuthCheck,
  Context (EmptyContext, (:.)),
  Handler (Handler),
  ServerT,
  hoistServerWithContext,
  serveWithContext,
 )
import Servant.Server.Generic (genericServerT)

mkAppEnv :: Config -> IO Env
mkAppEnv Config {..} = do
  envDbConnection <-
    Connection.acquire cfgDbConnectionString
      >>= either (throwM . DbConnectionAcquireException) pure
  let envOgmiosInfo = OgmiosInfo cfgOgmiosPort cfgOgmiosAddress
  envOgmiosWorker <- createStoppedFetcher
  let envControlApiToken = ControlApiToken cfgServerControlApiToken
  return Env {..}

newtype DbConnectionAcquireException
  = DbConnectionAcquireException Hasql.ConnectionError
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

appService :: Env -> Application
appService env =
  serveWithContext datumCacheApi serverContext appServer
  where
    appServer :: ServerT (ToServantApi Routes) Handler
    appServer =
      hoistServerWithContext
        datumCacheApi
        datumCacheContext
        hoistApp
        appServerT

    serverContext :: Context '[BasicAuthCheck ControlApiAuthData]
    serverContext = controlApiAuthCheck env :. EmptyContext

    hoistApp :: App a -> Handler a
    hoistApp = Handler . ExceptT . try . runStdoutLoggingT . flip runReaderT env . unApp

    appServerT :: ServerT (ToServantApi Routes) App
    appServerT = genericServerT datumServiceHandlers
