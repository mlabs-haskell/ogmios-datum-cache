module Main (main) where

import Control.Monad.Catch (catch)
import Data.Either (partitionEithers)
import Data.List (intercalate)
import Test.Hspec (hspec)

import App (
  DbConnectionAcquireException (),
  appService,
  mkAppEnv,
 )
import Config (Config (cfgServerControlApiToken), loadConfig)
import Parameters (Parameters (Parameters))

import Spec.Api.Handlers qualified
import Spec.Api.WebSocket.Types qualified
import Spec.Config qualified

main :: IO ()
main = do
  cfg <- loadConfig $ Parameters "config.toml"
  app <-
    (Right . appService <$> mkAppEnv cfg)
      `catch` (return . Left . show @DbConnectionAcquireException)
  appWithAuth <-
    (Right . appService <$> mkAppEnv cfg {cfgServerControlApiToken = Just "test:test"})
      `catch` (return . Left . show @DbConnectionAcquireException)
  let apps = case partitionEithers [app, appWithAuth] of
        ([], [app', appWithAuth']) -> Right (app', appWithAuth')
        (errs, _) -> Left $ intercalate "; " errs
  hspec $ do
    Spec.Api.WebSocket.Types.spec
    Spec.Config.spec
    Spec.Api.Handlers.spec apps
