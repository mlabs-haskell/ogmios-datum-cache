{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

module Config (
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
import Test.QuickCheck (
  Arbitrary,
  arbitrary,
  arbitrarySizedNatural,
  frequency,
 )

import App.Env (ControlApiToken (ControlApiToken, unControlApiToken))
import Block.Types (BlockInfo (BlockInfo))

data BlockFetcherConfig = BlockFetcherConfig
  { cfgFetcherBlock :: BlockInfo
  , cfgFetcherFilterJson :: Maybe LBS.ByteString
  , cfgFetcherUseLatest :: Bool
  , cfgFetcherQueueSize :: Natural
  }
  deriving stock (Show, Eq)

instance Arbitrary BlockFetcherConfig where
  arbitrary =
    BlockFetcherConfig
      <$> (BlockInfo <$> arbitrary <*> (Text.pack <$> arbitrary))
        <*> frequency [(8, Just . Text.Lazy.Encoding.encodeUtf8 . Text.Lazy.pack <$> arbitrary), (2, pure Nothing)]
        <*> arbitrary
        <*> arbitrarySizedNatural

data Config = Config
  { cfgDbConnectionString :: ByteString
  , cfgServerPort :: Int
  , cfgServerControlApiToken :: ControlApiToken
  , cfgOgmiosAddress :: String
  , cfgOgmiosPort :: Int
  , cfgFetcher :: BlockFetcherConfig
  }
  deriving stock (Show, Eq)

instance Arbitrary Config where
  arbitrary =
    Config
      <$> (Text.Encoding.encodeUtf8 . Text.pack <$> arbitrary)
      <*> arbitrary
      <*> (ControlApiToken <$> arbitrary)
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

configAsCLIOptions :: Config -> [String]
configAsCLIOptions Config {..} =
  let (BlockInfo slot hash) = cfgFetcher.cfgFetcherBlock
      useLatesString =
        if cfgFetcher.cfgFetcherUseLatest
          then "--useLatest"
          else ""
      mostParams =
        useLatesString :
        [ command "block-slot" slot
        , stringCommand "block-hash" $ Text.unpack hash
        , command "queueSize" cfgFetcher.cfgFetcherQueueSize
        , stringCommand "dbConnection" $ (Text.unpack . Text.Encoding.decodeUtf8) cfgDbConnectionString
        , command "serverPort" cfgServerPort
        , stringCommand "serverApi" $ unControlApiToken cfgServerControlApiToken
        , stringCommand "ogmiosAddress" cfgOgmiosAddress
        , command "ogmiosPort" cfgOgmiosPort
        ]
   in case cfgFetcher.cfgFetcherFilterJson of
        Just fil -> stringCommand "block-filter" ((Text.Lazy.unpack . Text.Lazy.Encoding.decodeUtf8) fil) : mostParams
        _ -> mostParams
  where
    command :: Show a => String -> a -> String
    command name x = "--" <> name <> "=" <> show x

    stringCommand :: String -> String -> String
    stringCommand name x =
      if x == ""
        then "--" <> name <> "=" <> "a"
        else "--" <> name <> "=" <> x
