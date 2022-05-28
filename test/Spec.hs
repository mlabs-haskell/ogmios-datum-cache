module Main (main) where

import Control.Monad.Catch (catch)
import Test.Hspec (hspec)

import App (
  DbConnectionAcquireException (),
  appService,
  mkAppEnv,
 )
import Config (loadConfig)
import Parameters (Parameters (Parameters))

import Spec.Api.Handlers qualified
import Spec.Api.WebSocket.Types qualified

main :: IO ()
main = do
  cfg <- loadConfig $ Parameters "config.toml"
  app <-
    (Right . appService <$> mkAppEnv cfg)
      `catch` (return . Left . show @DbConnectionAcquireException)
  hspec $ do
    Spec.Api.Handlers.spec app
    Spec.Api.WebSocket.Types.spec
