module Spec.Parameters (spec, example) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy.UTF8 (fromString)
import Options.Applicative (ParserResult (Success), defaultPrefs, execParserPure)
import Test.Hspec (Spec, describe, it, shouldBe)

import Block.Types (BlockInfo (BlockInfo, blockId, blockSlot), StartingBlock (StartingBlock))
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
  DBConnection (
    DBConnection,
    dbHost,
    dbName,
    dbPassword,
    dbPort,
    dbUser
  ),
  configAsCLIOptions,
  dbConnection2ByteString,
  parserInfo,
 )

spec :: Spec
spec = do
  describe "Config" $ do
    it "fixedConfig" $
      (parseParams . configAsCLIOptions) example `shouldBe` Right example
    it "DBConnection2string Non password" $
      dbConnection2ByteString (dbConnectionExample Nothing)
        `shouldBe` "port=5432 host=localhost user=seabug \
                   \dbname=ogmios-datum-cache"

    it "DBConnection2string with password" $
      dbConnection2ByteString (dbConnectionExample $ Just "fakePass")
        `shouldBe` "port=5432 host=localhost user=seabug \
                   \dbname=ogmios-datum-cache password=fakePass"

dbConnectionExample :: Maybe ByteString -> DBConnection
dbConnectionExample pass =
  DBConnection
    { dbPort = 5432
    , dbHost = "localhost"
    , dbUser = "seabug"
    , dbPassword = pass
    , dbName = "ogmios-datum-cache"
    }

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
              StartingBlock $
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
