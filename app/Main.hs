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
import Config (Config (..), loadConfig, cliParameters2Config, showConfigAsCLIOptions)
import Parameters (paramInfo)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  parameters <- paramInfo
  (Just (cfg@Config{..}) ) <-  case parameters of 
        Right cliParameters -> 
          pure $ cliParameters2Config cliParameters
        Left oldConfigOption -> 
          do 
          out <- loadConfig oldConfigOption
          runStdoutLoggingT $ do
            logWarnNS "ogmios-datum-cache" . Text.pack $ 
              ("The use of a toml file is deprecated, the new cli syntax for "
              <> "your current config file is :"
              <> "ogmios-datum-cache " <> showConfigAsCLIOptions out)
          return $ Just out
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
