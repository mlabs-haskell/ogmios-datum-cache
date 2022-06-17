module Parameters (
  argParser,
  parseArgs,
  parserInfo,
) where

import Block.Types (BlockInfo (BlockInfo))
import Config (
  BlockFetcherConfig (
    BlockFetcherConfig
  ),
  Config (Config),
 )
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
    <*> ( optional
            ( strOption
                ( long "block-filter"
                    <> metavar "FILTER"
                    <> help "Filter"
                )
            )
        )
    <*> switch
      ( long "useLatest"
          <> help "Use latest block"
      )
    <*> option
      auto
      ( long "queueSize"
          <> value 64
          <> metavar "NATURAL"
          <> help "Queue size"
      )

argParser :: Parser Config
argParser =
  Config
    <$> strOption
      ( long "dbConnection"
          <> help "Data base connection string"
      )
      <*> option
        auto
        ( long "serverPort"
            <> metavar "PORT"
            <> help "Server Port"
        )
      <*> strOption
        ( long "serverApi"
            <> metavar "SERVER_CONTROL_API_TOKEN"
            <> help "Token for server api"
        )
      <*> strOption
        ( long "ogmiosAddress"
            <> metavar "ADDRESS"
            <> help "Ogmios address"
        )
      <*> option
        auto
        ( long "ogmiosPort"
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
