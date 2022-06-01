module Main (main) where

import Control.Monad.Catch (catch)
import Data.Either (partitionEithers)
import Data.List (intercalate)
import System.Environment (lookupEnv)
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
  ci <- (== Just "true") <$> lookupEnv "CI"
  let handleDbException err
        | ci = error $ "Test environment is not running: " <> show err
        | otherwise = return $ Left $ show @DbConnectionAcquireException err
  cfg <- loadConfig $ Parameters "config.toml"
  app <- (Right . appService <$> mkAppEnv cfg) `catch` handleDbException
  appWithAuth <-
    (Right . appService <$> mkAppEnv cfg {cfgServerControlApiToken = Just "test:test"})
      `catch` handleDbException
  let apps = case partitionEithers [app, appWithAuth] of
        ([], [app', appWithAuth']) -> Right (app', appWithAuth')
        (errs, _) -> Left $ intercalate "; " errs
  hspec $ do
    Spec.Api.Handlers.spec apps
    Spec.Api.WebSocket.Types.spec
    Spec.Config.spec
