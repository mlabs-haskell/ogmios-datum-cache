module Main (
  main,
) where

import Control.Monad.Catch (Exception, throwM, try)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (eitherDecode)
import Data.Default (def)
import Data.Maybe (fromMaybe)
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
import Config (Config (..), loadConfig)
import Control.Concurrent (newEmptyMVar)
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
  Env pgConn (OgmiosInfo cfgOgmiosPort cfgOgmiosAddress) <$> newEmptyMVar

initDbAndFetcher :: Config -> Env -> IO Env
initDbAndFetcher cfg env =
  runStdoutLoggingT . flip runReaderT env $ do
    initTables
    let datumFilter' = case cfg.cfgFetcher.cfgFetcherFilterJson of
          Just filterJson -> eitherDecode filterJson
          Nothing -> pure def
    datumFilter <- case datumFilter' of
      Left e -> error $ show e
      Right x -> pure x
    latestBlock' <- getLastBlock env.envDbConnection
    let firstBlock =
          if cfg.cfgFetcher.cfgFetcherUseLatest
            then fromMaybe cfg.cfgFetcher.cfgFetcherBlock latestBlock'
            else cfg.cfgFetcher.cfgFetcherBlock
    initLastBlock firstBlock
    updateLastBlock env.envDbConnection firstBlock
    (blockFetcherEnv, _) <- startBlockFetcherAndProcessor env.envOgmiosInfo env.envDbConnection firstBlock datumFilter
    pure $ env {envBlockFetcherEnv = blockFetcherEnv}

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  parameters <- paramInfo
  cfg@Config {..} <- loadConfig parameters
  print cfg
  env <- mkAppEnv cfg >>= initDbAndFetcher cfg
  withStdoutLogger $ \logger -> do
    let warpSettings =
          Warp.setPort cfgServerPort $
            Warp.setLogger logger Warp.defaultSettings
    Warp.runSettings warpSettings $ simpleCors (appService env)
