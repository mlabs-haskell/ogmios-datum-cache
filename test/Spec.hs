module Main (main) where

import Control.Monad.Catch (catch)
import System.Environment (lookupEnv)
import Test.Hspec (hspec)

import App (
  DbConnectionAcquireException (),
  appService,
  bootstrapEnvFromConfig,
 )
import Codec.Serialise (Serialise (encode))
import Spec.Api.Handlers qualified
import Spec.Api.WebSocket.Types qualified
import Spec.Block.Parsers qualified
import Spec.Integration.CardanoNode (loockAndRelease)
import Spec.Parameters qualified
import Spec.Types (PlutusData (Constr))

main :: IO ()
main = do
  ci <- (== Just "true") <$> lookupEnv "CI"
  let handleDbException err
        | ci = error $ "Test environment is not running: " <> show err
        | otherwise = return $ Left $ show @DbConnectionAcquireException err
      cfg = Spec.Parameters.integrationTestParams
  loockAndRelease
  app <- (Right . appService <$> bootstrapEnvFromConfig cfg) `catch` handleDbException
  let dat = Constr 1 []
  print dat
  print $ encode dat
  hspec $ do
    -- Those won't depend on environment
    Spec.Parameters.spec
    Spec.Block.Parsers.spec
    Spec.Api.WebSocket.Types.spec

    -- Depends on environment
    Spec.Api.Handlers.spec app
