module Main (
  main,
) where

import Control.Monad (when)
import Control.Monad.Logger (logErrorNS, logInfoNS, logWarnNS, runStdoutLoggingT)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (eitherDecode)
import Data.Default (def)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors (simpleCors)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

import App (appService, mkAppEnv)
import App.Env (Env (..))
import Block.Fetch (startBlockFetcher)
import Config (BlockFetcherConfig (BlockFetcherConfig), Config (..), loadConfig)
import Database (getLastBlock, initLastBlock, initTables, updateLastBlock)
import Parameters (paramInfo)

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
  runStdoutLoggingT $ do
    logInfoNS "ogmios-datum-cache" $ Text.pack $ show cfg
    when (cfgServerControlApiToken == "usr:pwd") $
      logWarnNS
        "ogmios-datum-cache"
        "Using default auth configuration is UNSAFE! Change 'server.controlApiToken'!"
  env <- mkAppEnv cfg
  initDbAndFetcher env cfg
  withStdoutLogger $ \logger -> do
    let warpSettings =
          Warp.setPort cfgServerPort $
            Warp.setLogger logger Warp.defaultSettings
    Warp.runSettings warpSettings $ simpleCors $ appService env
