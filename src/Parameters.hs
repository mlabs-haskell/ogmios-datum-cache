module Parameters (
  argParser,
  parseArgs,
  parserInfo,
  Config (..),
  BlockFetcherConfig (..),
  configAsCLIOptions,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Encoding qualified as Text.Lazy.Encoding
import GHC.Natural (Natural)

import App.Env (ControlApiToken (unControlApiToken))
import Block.Types (BlockInfo (BlockInfo))
import Options.Applicative (
  Parser,
  ParserInfo,
  auto,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  optional,
  strOption,
  switch,
  value,
  (<**>),
 )

data BlockFetcherConfig = BlockFetcherConfig
  { cfgFetcherBlock :: BlockInfo
  , cfgFetcherFilterJson :: Maybe LBS.ByteString
  , cfgFetcherUseLatest :: Bool
  , cfgFetcherQueueSize :: Natural
  }
  deriving stock (Show, Eq)

data Config = Config
  { cfgDbConnectionString :: ByteString
  , cfgServerPort :: Int
  , cfgServerControlApiToken :: ControlApiToken
  , cfgOgmiosAddress :: String
  , cfgOgmiosPort :: Int
  , cfgFetcher :: BlockFetcherConfig
  }
  deriving stock (Show, Eq)

parseFirstBlock :: Parser BlockInfo
parseFirstBlock =
  BlockInfo
    <$> option
      auto
      ( long
          "block-slot"
          <> metavar
            "INT"
          <> help "Block slot"
      )
    <*> strOption
      ( long "block-hash"
          <> metavar "HASH"
          <> help "Block hash"
      )

parseBlockFetcher :: Parser BlockFetcherConfig
parseBlockFetcher =
  BlockFetcherConfig
    <$> parseFirstBlock
    <*> optional
      ( strOption
          ( long "block-filter"
              <> metavar "FILTER"
              <> help "Filter"
          )
      )
    <*> switch
      ( long "use-latest"
          <> help "Use latest block"
      )
    <*> option
      auto
      ( long "queue-size"
          <> value 64
          <> metavar "NATURAL"
          <> help "Queue size"
      )

argParser :: Parser Config
argParser =
  Config
    <$> strOption
      ( long "db-connection"
          <> help "Data base connection string"
      )
      <*> option
        auto
        ( long "server-port"
            <> metavar "PORT"
            <> help "Server Port"
        )
      <*> strOption
        ( long "server-api"
            <> metavar "SERVER_CONTROL_API_TOKEN"
            <> help "Token for server api"
        )
      <*> strOption
        ( long "ogmios-address"
            <> metavar "ADDRESS"
            <> help "Ogmios address"
        )
      <*> option
        auto
        ( long "ogmios-port"
            <> metavar "PORT"
            <> help "Ogmios port"
        )
      <*> parseBlockFetcher

parserInfo :: ParserInfo Config
parserInfo =
  info
    (argParser <**> helper)
    (fullDesc <> header "Ogmios Datum Cache")

parseArgs :: IO Config
parseArgs = execParser parserInfo

configAsCLIOptions :: Config -> [String]
configAsCLIOptions Config {..} =
  let (BlockInfo slot hash) = cfgFetcher.cfgFetcherBlock
      useLatesString =
        if cfgFetcher.cfgFetcherUseLatest
          then ["--use-latest"]
          else []
      mostParams =
        useLatesString
          <> [ command "block-slot" slot
             , stringCommand "block-hash" $ Text.unpack hash
             , command "queue-size" cfgFetcher.cfgFetcherQueueSize
             , stringCommand "db-connection" $
                (Text.unpack . Text.Encoding.decodeUtf8) cfgDbConnectionString
             , command "server-port" cfgServerPort
             , stringCommand "server-api" $
                unControlApiToken cfgServerControlApiToken
             , stringCommand "ogmios-address" cfgOgmiosAddress
             , command "ogmios-port" cfgOgmiosPort
             ]
   in maybe
        mostParams
        ( (: mostParams)
            . stringCommand "block-filter"
            . Text.Lazy.unpack
            . Text.Lazy.Encoding.decodeUtf8
        )
        cfgFetcher.cfgFetcherFilterJson
  where
    command :: Show a => String -> a -> String
    command name x = "--" <> name <> "=" <> show x

    stringCommand :: String -> String -> String
    stringCommand name x = "--" <> name <> "=" <> x
