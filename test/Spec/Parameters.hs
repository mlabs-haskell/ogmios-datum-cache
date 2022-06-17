module Spec.Parameters (spec, example) where

import Data.ByteString.Lazy.UTF8 (fromString)
import Options.Applicative (ParserResult (Success), defaultPrefs, execParserPure)
import Test.Hspec (Spec, describe, it, shouldBe)

import Block.Types (BlockInfo (BlockInfo, blockId, blockSlot))
import Parameters (
  BlockFetcherConfig (
    BlockFetcherConfig,
    cfgFetcherBlock,
    cfgFetcherFilterJson,
    cfgFetcherQueueSize,
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
  configAsCLIOptions,
  parserInfo,
 )

spec :: Spec
spec = do
  describe "Config" $ do
    it "fixedConfig" $
      (parseParams . configAsCLIOptions) example `shouldBe` Right example

example :: Config
example =
  Config
    { cfgDbConnectionString =
        "host=localhost port=5432 user=seabug dbname=ogmios-datum-cache"
    , cfgServerPort = 9999
    , cfgServerControlApiToken = "API_TOKEN"
    , cfgOgmiosAddress = "127.0.0.1"
    , cfgOgmiosPort = 1337
    , cfgFetcher =
        BlockFetcherConfig
          { cfgFetcherBlock =
              BlockInfo
                { blockSlot = 44366242
                , blockId =
                    "d2a4249fe3d0607535daa26caf12a38da2233586bc51e79ed0b3a36170471bf5"
                }
          , cfgFetcherFilterJson =
              (Just . fromString)
                "{\
                \    \"all\": [\
                \        {\
                \            \"hash\": \"foobar\"\
                \        },\
                \        {\
                \            \"any\": [\
                \                { \"address\": \"addr_abc\" },\
                \                { \"address\": \"addr_xyz\" }\
                \            ]\
                \        }\
                \    ]\
                \}"
          , cfgFetcherUseLatest = True
          , cfgFetcherQueueSize = 64
          }
    }

parseParams :: [String] -> Either String Config
parseParams strs =
  case execParserPure defaultPrefs parserInfo strs of
    Success conf -> Right conf
    a -> Left $ show a
