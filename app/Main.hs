module Main (
  main,
) where

import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors (simpleCors)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

import App (appService, bootstrapEnvFromConfig)
import Config (Config (..), loadConfig)
import Parameters (paramInfo)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  parameters <- paramInfo
  cfg@Config {..} <- loadConfig parameters
  print cfg
  env <- bootstrapEnvFromConfig cfg
  withStdoutLogger $ \logger -> do
    let warpSettings =
          Warp.setPort cfgServerPort $
            Warp.setLogger logger Warp.defaultSettings
    Warp.runSettings warpSettings $ simpleCors $ appService env
