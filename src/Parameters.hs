module Parameters (
  Parameters (..),
  argParser,
  paramInfo,
  OldConfigOption (..),
  oldConfigParser,
  BlockFetcherParameters (..),
  OgmiosParameters (..),
  ServerParameters (..),
) where

import App.Env (ControlApiToken)
import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Natural (Natural)
import Options.Applicative (
  Parser,
  auto,
  execParser,
  flag',
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  short,
  str,
  strOption,
  switch,
  value,
  (<**>),
 )

data OldConfigOption = OldConfigOption {config :: FilePath}

data BlockFetcherParameters = BlockFetcherParameters
  { blockInfo :: Maybe (Int64, Text)
  , blockFilter :: Maybe LBS.ByteString
  , useLatest :: Bool
  , queueSize :: Natural
  }
  deriving stock (Eq, Show)

parseFirstBlock :: Parser (Maybe (Int64, Text))
parseFirstBlock =
  Just
    <$> ( (,)
            <$> option
              auto
              ( long "block-slot"
                  <> help "Block slot"
              )
            <*> strOption
              ( long "block-hash"
                  <> help "Block hash"
              )
        )

parseFirstBlockOrigin :: Parser (Maybe (Int64, Text))
parseFirstBlockOrigin =
  flag'
    Nothing
    ( long "origin"
        <> help "Use the origin block"
    )

parseBlockFetcher :: Parser BlockFetcherParameters
parseBlockFetcher =
  BlockFetcherParameters
    <$> (parseFirstBlock <|> parseFirstBlockOrigin)
    <*> ( Just
            <$> strOption
              ( long "block-filter"
                  <> metavar "Filter"
                  <> help "Filter"
              )
            <|> pure Nothing
        )
    <*> switch
      ( long "useLastest"
          <> help "Use latests block"
      )
    <*> option
      auto
      ( long "queueSize"
          <> value 64
          <> metavar "Natural"
          <> help "Queue size"
      )

parseDBParameters :: Parser ByteString
parseDBParameters =
  strOption
    ( long "DBConnection"
        <> help "DB connection string"
    )

data ServerParameters = ServerParameters
  { serverPort :: Int
  , serverControlApiToken :: ControlApiToken
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
      ( long "serverApi"
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

data Parameters = Parameters
  { dbConnectionString :: ByteString
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
