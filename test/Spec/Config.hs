{-# LANGUAGE QuasiQuotes #-}

module Spec.Config (spec) where

import Data.String.Interpolate (i)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe)

import Config (
  Config (
    Config,
    cfgDbConnectionString,
    cfgFetcher,
    cfgOgmiosAddress,
    cfgOgmiosPort,
    cfgServerControlApiToken,
    cfgServerPort
  ),
  loadConfig,
 )
import Parameters (Parameters (Parameters))

spec :: Spec
spec = do
  describe "Config" $ do
    it "simplest" $ do
      [i|
             dbConnectionString = "host=localhost port=5432 user=aske dbname=ogmios-datum-cache"
             server.port = 9999
             ogmios.address = "127.0.0.1"
             ogmios.port = 1337
      |]
        `configShouldBe` example
    it "withApiToken" $ do
      [i|
             dbConnectionString = "host=localhost port=5432 user=aske dbname=ogmios-datum-cache"
             server.port = 9999
             server.controlApiToken = "API_TOKEN"
             ogmios.address = "127.0.0.1"
             ogmios.port = 1337
      |]
        `configShouldBe` example {cfgServerControlApiToken = Just "API_TOKEN"}

example :: Config
example =
  Config
    { cfgDbConnectionString = "host=localhost port=5432 user=aske dbname=ogmios-datum-cache"
    , cfgServerPort = 9999
    , cfgServerControlApiToken = Nothing
    , cfgOgmiosAddress = "127.0.0.1"
    , cfgOgmiosPort = 1337
    , cfgFetcher = Nothing
    }

configShouldBe :: String -> Config -> IO ()
toml `configShouldBe` conf = do
  wd <- getCurrentDirectory
  withSystemTempDirectory "ogmios-datum-cache-test" $ \path -> do
    setCurrentDirectory path
    writeFile "config.toml" toml
    loadConfig (Parameters "config.toml") >>= (`shouldBe` conf)
  setCurrentDirectory wd
