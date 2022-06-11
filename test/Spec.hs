module Main (main) where

import Control.Monad.Catch (catch)
import System.Environment (lookupEnv)
import Test.Hspec (hspec)

import App (
  DbConnectionAcquireException (),
  appService,
  bootstrapEnvFromConfig,
 )
import Config (loadConfig)
import Parameters (Parameters (Parameters))

import Spec.Api.Handlers qualified
import Spec.Api.WebSocket.Types qualified

main :: IO ()
main = do
  ci <- (== Just "true") <$> lookupEnv "CI"
  let handleDbException err
        | ci = error $ "Test environment is not running: " <> show err
        | otherwise = return $ Left $ show @DbConnectionAcquireException err
  cfg <- loadConfig $ Parameters "config.toml"
  app <- (Right . appService <$> bootstrapEnvFromConfig cfg) `catch` handleDbException
  hspec $ do
    Spec.Api.Handlers.spec app
    Spec.Api.WebSocket.Types.spec
