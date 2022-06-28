module Parameters (
  argParser,
  parseArgs,
  parserInfo,
  Config (..),
  BlockFetcherConfig (..),
  DBConnection (..),
  dbConnection2ByteString,
  configAsCLIOptions,
) where

import Control.Monad.Logger (
  LogLevel (
    LevelDebug,
    LevelError,
    LevelInfo,
    LevelWarn
  ),
 )
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.List (intersperse)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Encoding qualified as Text.Lazy.Encoding
import GHC.Natural (Natural)
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
  maybeReader,
  metavar,
  option,
  optional,
  strOption,
  switch,
  value,
  (<**>),
  (<|>),
 )

import App.Env (ControlApiToken (unControlApiToken))
import Block.Types (BlockInfo (BlockInfo), StartingBlock (Origin, StartingBlock))

data BlockFetcherConfig = BlockFetcherConfig
  { cfgFetcherBlock :: StartingBlock
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
  , cfgLogLevel :: LogLevel
  }
  deriving stock (Show, Eq)

data DBConnection = DBConnection
  { dbPort :: Int
  , dbHost :: ByteString
  , dbUser :: ByteString
  , dbPassword :: Maybe ByteString
  , dbName :: ByteString
  }
  deriving stock (Show, Eq)

dbConnection2ByteString :: DBConnection -> ByteString
dbConnection2ByteString DBConnection {..} =
  unwords'
    [ "port=" <> toBytes (show dbPort)
    , "host=" <> dbHost
    , "user=" <> dbUser
    , "dbname=" <> dbName
    ]
    <> foldMap (" password=" <>) dbPassword
  where
    unwords' = mconcat . intersperse (toBytes " ")
    toBytes :: String -> ByteString
    toBytes = Text.Encoding.encodeUtf8 . Text.pack

parseOrigin :: Parser StartingBlock
parseOrigin = Origin <$ switch (long "origin" <> help "Start block fetcher from origin")

parseFirstBlock :: Parser StartingBlock
parseFirstBlock =
  fmap StartingBlock $
    BlockInfo
      <$> option
        auto
        ( long
            "block-slot"
            <> metavar
              "INT"
            <> help "Slot of first block to fetch by initial block fetcher."
        )
      <*> strOption
        ( long "block-hash"
            <> metavar "HASH"
            <> help "hash of block's HEADER not hash of a block itself"
        )

parseBlockFetcher :: Parser BlockFetcherConfig
parseBlockFetcher =
  BlockFetcherConfig
    <$> (parseFirstBlock <|> parseOrigin)
    <*> optional
      ( strOption
          ( long "block-filter"
              <> metavar "FILTER"
              <> help "Filter."
          )
      )
    <*> switch
      ( long "use-latest"
          <> help
            "defines if block fetcher, if started automatically, should \
            \start from last block that was proccessed rather than from \
            \block defined with --block-slot and --block-hash."
      )
    <*> option
      auto
      ( long "queue-size"
          <> value 64
          <> metavar "NATURAL"
          <> help
            "Defines size of queue of prefetched blocks ready to be \
            \processed, default=64."
      )

parseDBConnection :: Parser DBConnection
parseDBConnection =
  DBConnection
    <$> option
      auto
      ( long "db-port"
          <> metavar "PORT"
          <> help "Postgres libpq connection port"
      )
    <*> strOption
      ( long "db-host"
          <> metavar "HOST_NAME"
          <> help "Postgres libpq connection host"
      )
    <*> strOption
      ( long "db-user"
          <> metavar "USER_NAME"
          <> help "Postgres libpq connection user"
      )
    <*> optional
      ( strOption
          ( long "db-password"
              <> metavar "PASSWORD"
              <> help "Postgres libpq connection password"
          )
      )
    <*> strOption
      ( long "db-name"
          <> metavar "DB_NAME"
          <> help "Postgres libpq connection data base name"
      )

parseDBConnectionString :: Parser ByteString
parseDBConnectionString =
  strOption
    ( long "db-connection"
        <> metavar "POSTGRES_LIBPQ_CONNECTION_STRING"
        <> help "\"host=localhost port=5432 user=<user> password=<pass>\""
    )

parseLogLevel :: Parser LogLevel
parseLogLevel =
  option
    (maybeReader validateLevel)
    ( long "log-level"
        <> metavar "LOG_LEVEL"
        <> value LevelWarn
        <> help
          "One of [debug | info | warn | error], every level\
          \ is more restrictive than the previous level. By default set to warn"
    )
  where
    validateLevel str
      | str == "info" = pure LevelInfo
      | str == "debug" = pure LevelDebug
      | str == "warn" = pure LevelWarn
      | str == "error" = pure LevelError
      | otherwise = Nothing

argParser :: Parser Config
argParser =
  Config
    <$> ( (dbConnection2ByteString <$> parseDBConnection)
            <|> parseDBConnectionString
        )
    <*> option
      auto
      ( long "server-port"
          <> metavar "PORT"
          <> help "ODC server port"
      )
    <*> strOption
      ( long "server-api"
          <> metavar "SERVER_CONTROL_API_TOKEN"
          <> help "Defines the secrete token, required for control API call. Format: user:password"
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
    <*> parseLogLevel

parserInfo :: ParserInfo Config
parserInfo =
  info
    (argParser <**> helper)
    (fullDesc <> header "Ogmios Datum Cache")

parseArgs :: IO Config
parseArgs = execParser parserInfo

configAsCLIOptions :: Config -> [String]
configAsCLIOptions Config {..} =
  let blockOptions = case cfgFetcher.cfgFetcherBlock of
        StartingBlock (BlockInfo slot hash) ->
          [ command "block-slot" slot
          , stringCommand "block-hash" $ Text.unpack hash
          ]
        Origin -> ["--origin"]
      useLatesString = ["--use-latest" | cfgFetcher.cfgFetcherUseLatest]
      logLevel =
        case cfgLogLevel of
          LevelInfo -> ["--log-level=info"]
          LevelDebug -> ["--log-level=debug"]
          LevelWarn -> ["--log-level=warn"]
          LevelError -> ["--log-level=error"]
          _ -> []

      mostParams =
        useLatesString
          <> blockOptions
          <> logLevel
          <> [ command "queue-size" cfgFetcher.cfgFetcherQueueSize
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
