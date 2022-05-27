{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Monad.Catch (catch)
import Test.Hspec (hspec)

import App (
  DbConnectionAcquireException (DbConnectionAcquireException),
  appService,
  mkAppEnv,
 )
import Config (loadConfig)
import Parameters (Parameters (Parameters))

import Spec.Api.Handlers qualified
import Spec.Api.WebSocket.Types qualified
import Spec.Config qualified

main :: IO ()
main = do
  app <-
    ( do
        cfg <- loadConfig $ Parameters "config.toml"
        app <- appService <$> mkAppEnv cfg
        return $ Right app
      )
      `catch` (\err@DbConnectionAcquireException {} -> return $ Left $ show err)
  hspec $ do
    Spec.Api.WebSocket.Types.spec
    Spec.Config.spec
    Spec.Api.Handlers.spec app
