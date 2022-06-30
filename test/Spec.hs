module Main (main) where

import Control.Monad.Catch (catch)
import System.Environment (lookupEnv)
import Test.Hspec (hspec)

import App (
  DbConnectionAcquireException (),
  appService,
  bootstrapEnvFromConfig,
 )
import Spec.Api.Handlers qualified
import Spec.Api.WebSocket.Types qualified
import Spec.Parameters qualified

main :: IO ()
main = do
  ci <- (== Just "true") <$> lookupEnv "CI"
  let handleDbException err
        | ci = error $ "Test environment is not running: " <> show err
        | otherwise = return $ Left $ show @DbConnectionAcquireException err
      cfg = Spec.Parameters.example
  app <- (Right . appService <$> bootstrapEnvFromConfig cfg) `catch` handleDbException
  hspec $ do
    Spec.Api.Handlers.spec app
    Spec.Api.WebSocket.Types.spec
    Spec.Parameters.spec
