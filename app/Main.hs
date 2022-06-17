module Main (
  main,
) where

import Control.Monad (when)
import Control.Monad.Logger (logInfoNS, logWarnNS, runStdoutLoggingT)
import Data.Text qualified as Text
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors (simpleCors)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

import App (appService, bootstrapEnvFromConfig)
import Config (Config (Config, cfgServerControlApiToken, cfgServerPort))
import Parameters (parseArgs)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  cfg@Config {..} <- parseArgs
  runStdoutLoggingT $ do
    logInfoNS "ogmios-datum-cache" $ Text.pack $ show cfg
    when (cfgServerControlApiToken == "usr:pwd") $
      logWarnNS
        "ogmios-datum-cache"
        "Using default auth configuration is UNSAFE! Change 'server.controlApiToken'!"
  env <- bootstrapEnvFromConfig cfg
  withStdoutLogger $ \logger -> do
    let warpSettings =
          Warp.setPort cfgServerPort $
            Warp.setLogger logger Warp.defaultSettings
    Warp.runSettings warpSettings $ simpleCors $ appService env
