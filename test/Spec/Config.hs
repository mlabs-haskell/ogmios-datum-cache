{-# LANGUAGE QuasiQuotes #-}

module Spec.Config (spec) where

import Data.String.Interpolate (i)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe)

import Block.Types (BlockInfo (BlockInfo, blockId, blockSlot))
import Config (
  BlockFetcherConfig (
    BlockFetcherConfig,
    cfgFetcherBlock,
    cfgFetcherFilterJson,
    cfgFetcherUseLatest
  ),
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
             server.controlApiToken = "API_TOKEN"
             ogmios.address = "127.0.0.1"
             ogmios.port = 1337
             blockFetcher.autoStart = true
             blockFetcher.startFromLast = true
             blockFetcher.firstBlock.slot = 44366242
             blockFetcher.firstBlock.id = "d2a4249fe3d0607535daa26caf12a38da2233586bc51e79ed0b3a36170471bf5"
      |]
        `configShouldBe` example

example :: Config
example =
  Config
    { cfgDbConnectionString = "host=localhost port=5432 user=aske dbname=ogmios-datum-cache"
    , cfgServerPort = 9999
    , cfgServerControlApiToken = "API_TOKEN"
    , cfgOgmiosAddress = "127.0.0.1"
    , cfgOgmiosPort = 1337
    , cfgFetcher =
        Just
          BlockFetcherConfig
            { cfgFetcherBlock =
                BlockInfo
                  { blockSlot = 44366242
                  , blockId = "d2a4249fe3d0607535daa26caf12a38da2233586bc51e79ed0b3a36170471bf5"
                  }
            , cfgFetcherFilterJson = Nothing
            , cfgFetcherUseLatest = True
            }
    }

configShouldBe :: String -> Config -> IO ()
toml `configShouldBe` conf = do
  wd <- getCurrentDirectory
  withSystemTempDirectory "ogmios-datum-cache-test" $ \path -> do
    setCurrentDirectory path
    writeFile "config.toml" toml
    loadConfig (Parameters "config.toml") >>= (`shouldBe` conf)
  setCurrentDirectory wd
