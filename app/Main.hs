module Main (
  main,
) where

import Control.Monad.Catch (Exception, throwM, try)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.Logger (logErrorNS, logInfoNS, runStdoutLoggingT)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (eitherDecode)
import Data.Default (def)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Hasql.Connection qualified as Connection
import Hasql.Connection qualified as Hasql
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant.API.Generic (ToServantApi)
import Servant.Server (
  Application,
  BasicAuthCheck,
  Context (EmptyContext, (:.)),
  Handler (..),
  ServerT,
  hoistServerWithContext,
  serveWithContext,
 )
import Servant.Server.Generic (genericServerT)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

import Api (Routes, datumCacheApi, datumCacheContext)
import Api.Handler (controlApiAuthCheck, datumServiceHandlers)
import Api.Types (ControlApiAuthData)
import App (App (..))
import App.Env (ControlApiToken (ControlApiToken), Env (..))
import Block.Fetch (
  OgmiosInfo (OgmiosInfo),
  createStoppedFetcher,
  startBlockFetcher,
 )
import Config (BlockFetcherConfig (BlockFetcherConfig), Config (..), loadConfig)
import Database (getLastBlock, initLastBlock, initTables, updateLastBlock)
import Parameters (paramInfo)

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

newtype DbConnectionAcquireException
  = DbConnectionAcquireException Hasql.ConnectionError
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

mkAppEnv :: Config -> IO Env
mkAppEnv Config {..} = do
  envDbConnection <-
    Connection.acquire cfgDbConnectionString
      >>= either (throwM . DbConnectionAcquireException) pure
  let envOgmiosInfo = OgmiosInfo cfgOgmiosPort cfgOgmiosAddress
  envOgmiosWorker <- createStoppedFetcher
  let envControlApiToken = ControlApiToken cfgServerControlApiToken
  return Env {..}

initDbAndFetcher :: Env -> Config -> IO ()
initDbAndFetcher env Config {..} =
  runStdoutLoggingT . flip runReaderT env $ do
    initTables
    case cfgFetcher of
      Nothing -> pure ()
      Just (BlockFetcherConfig blockInfo filterJson' useLatest) -> do
        let datumFilter' = case filterJson' of
              Just filterJson -> eitherDecode filterJson
              Nothing -> pure def
        case datumFilter' of
          Left e -> logErrorNS "initDbAndFetcher" $ Text.pack $ show e
          Right datumFilter -> do
            logInfoNS "initDbAndFetcher" $
              Text.pack $ "Filter: " <> show datumFilter
            latestBlock' <- getLastBlock
            let firstBlock =
                  if useLatest
                    then fromMaybe blockInfo latestBlock'
                    else blockInfo
            initLastBlock firstBlock
            updateLastBlock firstBlock
            r <- startBlockFetcher firstBlock datumFilter
            case r of
              Right () -> pure ()
              Left e -> logErrorNS "initDbAndFetcher" $ Text.pack $ show e

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  parameters <- paramInfo
  cfg@Config {..} <- loadConfig parameters
  print cfg
  env <- mkAppEnv cfg
  initDbAndFetcher env cfg
  withStdoutLogger $ \logger -> do
    let warpSettings =
          Warp.setPort cfgServerPort $
            Warp.setLogger logger Warp.defaultSettings
    Warp.runSettings warpSettings $ simpleCors (appService env)
