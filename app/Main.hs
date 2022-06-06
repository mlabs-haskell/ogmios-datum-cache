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
import Servant.Server (Application, Handler (..), ServerT, hoistServer, serve)
import Servant.Server.Generic (genericServerT)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

import Api (Routes, datumCacheApi)
import Api.Handler (datumServiceHandlers)
import App (App (..))
import App.Env (Env (..))
import Block.Fetch (
  OgmiosInfo (OgmiosInfo),
  startBlockFetcherAndProcessor,
 )
import Config (BlockFetcherConfig (BlockFetcherConfig), Config (..), loadConfig)
import Database (getLastBlock, initLastBlock, initTables, updateLastBlock)
import Parameters (paramInfo)

appService :: Env -> Application
appService env = serve datumCacheApi appServer
  where
    appServer :: ServerT (ToServantApi Routes) Handler
    appServer = hoistServer datumCacheApi hoistApp appServerT

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
  pgConn <-
    Connection.acquire cfgDbConnectionString
      >>= either (throwM . DbConnectionAcquireException) pure
  pure $ Env pgConn (OgmiosInfo cfgOgmiosPort cfgOgmiosAddress)

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
            latestBlock' <- getLastBlock env.envDbConnection
            let firstBlock =
                  if useLatest
                    then fromMaybe blockInfo latestBlock'
                    else blockInfo
            initLastBlock firstBlock
            updateLastBlock env.envDbConnection firstBlock
            startBlockFetcherAndProcessor env.envOgmiosInfo env.envDbConnection firstBlock datumFilter

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
