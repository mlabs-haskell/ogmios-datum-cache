module Parameters (
  Parameters (..),
  argParser,
  paramInfo,
  OldConfigOption (..),
  oldConfigParser,
) where

import Control.Applicative ((<|>))
import Options.Applicative (
  Parser,
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
  short,
  strOption,
  switch,
  value,
  (<**>),
 )

data OldConfigOption = OldConfigOption {config :: FilePath}

data DBParameters = DBParameters
  { dbHost :: String
  , dbPort :: Int
  , dbUser :: String
  , dbName :: String
  }
  deriving stock (Eq, Show)

parseDBParameters :: Parser DBParameters
parseDBParameters =
  DBParameters
    <$> strOption
      ( long "dbHost"
          <> metavar "HostName"
          <> help "host name of the data base"
      )
    <*> option
      auto
      ( long "dbPort"
          <> metavar "Port"
          <> help "Data base port"
      )
    <*> strOption
      ( long "dbUser"
          <> metavar "DBUser"
          <> help "User for data base"
      )
    <*> strOption
      ( long "dbName"
          <> metavar "DBName"
          <> help "Data base name"
      )

data ServerParameters = ServerParameters
  { serverPort :: Int
  , serverControlApiToken :: String
  }
  deriving stock (Eq, Show)

parseServerParameters :: Parser ServerParameters
parseServerParameters =
  ServerParameters
    <$> option
      auto
      ( long "serverPort"
          <> metavar "Port"
          <> help "Server Port"
      )
    <*> strOption
      ( long "serverControl"
          <> metavar "ServerControlApiToken"
          <> help "Token for server api"
      )

data OgmiosParameters = OgmiosParameters
  { ogmiosAddress :: String
  , ogmiosPort :: Int
  }
  deriving stock (Eq, Show)

parseOgmios :: Parser OgmiosParameters
parseOgmios =
  OgmiosParameters
    <$> strOption
      ( long "ogmiosAddress"
          <> metavar "Address"
          <> help "Ogmios address"
      )
    <*> option
      auto
      ( long "ogmiosPort"
          <> metavar "Port"
          <> help "Ogmios port"
      )

data BlockFetcherParameters = BlockFetcherParameters
  { autoStart :: Bool
  , startFromLast :: Bool
  , firstBlockSlot :: Int
  , firstBlockId :: String
  , filter :: String
  }
  deriving stock (Eq, Show)

parseBlockFetcher :: Parser BlockFetcherParameters
parseBlockFetcher =
  BlockFetcherParameters
    <$> switch
      ( long "autoStart"
          <> help "Block fetcher autoStart flag"
      )
    <*> switch
      ( long "startFromLast"
          <> help "Block fetcher start from last option"
      )
    <*> option
      auto
      ( long "firstBlockSlot"
          <> metavar "BlockSlot"
          <> help "First block slot"
      )
    <*> strOption
      ( long "firstBlockId"
          <> metavar "BlockID"
          <> help "First block id"
      )
    <*> strOption
      ( long "filter"
          <> metavar "Filter"
          <> help "Filter"
      )

data Parameters = Parameters
  { dbParameters :: DBParameters
  , serverParameters :: ServerParameters
  , ogmiosParameters :: OgmiosParameters
  , blockFetcherParameters :: BlockFetcherParameters
  }
  deriving stock (Eq, Show)

argParser :: Parser Parameters
argParser =
  Parameters
    <$> parseDBParameters
    <*> parseServerParameters
    <*> parseOgmios
    <*> parseBlockFetcher

oldConfigParser :: Parser OldConfigOption
oldConfigParser =
  OldConfigOption
    <$> strOption
      ( long "config"
          <> metavar "FILEPATH [DEPRECATED]"
          <> help "filepath where config is loaded from [DEPRECATED]"
      )

paramInfo :: IO (Either OldConfigOption Parameters)
paramInfo = execParser (info ((Right <$> argParser) <|> (Left <$> oldConfigParser) <**> helper) (fullDesc <> header "Ogmios Datum Cache"))
