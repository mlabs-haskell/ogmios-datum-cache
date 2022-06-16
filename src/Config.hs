{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

module Config (loadConfig, Config (..), BlockFetcherConfig (..)) where

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.String.ToString (toString)
import GHC.Natural (Natural)
import Parameters (OldConfigOption (OldConfigOption, config), Parameters (Parameters))
import Toml (TomlCodec, dimap, dioptional, (.=))
import Toml qualified

import App.Env (ControlApiToken (ControlApiToken))
import Block.Types (BlockInfo (BlockInfo), blockId, blockSlot)

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

withDefault :: a -> TomlCodec a -> TomlCodec a
withDefault d c = dimap pure (fromMaybe d) (Toml.dioptional c)

int64 :: Toml.Key -> TomlCodec Int64
int64 k = dimap fromIntegral fromIntegral (Toml.integer k)

blockInfoT :: TomlCodec BlockInfo
blockInfoT = do
  blockSlot' <-
    int64 "blockFetcher.firstBlock.slot"
      .= blockSlot
  blockId' <-
    Toml.text "blockFetcher.firstBlock.id"
      .= blockId
  pure $ BlockInfo blockSlot' blockId'

withFetcherT :: TomlCodec BlockFetcherConfig
withFetcherT = do
  cfgFetcherFilterJson <-
    Toml.dioptional (Toml.lazyByteString "blockFetcher.filter")
      .= cfgFetcherFilterJson
  cfgFetcherBlock <- blockInfoT .= cfgFetcherBlock
  cfgFetcherUseLatest <-
    withDefault False (Toml.bool "blockFetcher.startFromLast")
      .= cfgFetcherUseLatest
  cfgFetcherQueueSize <-
    withDefault 64 (Toml.natural "blockFetcher.queueSize") .= cfgFetcherQueueSize
  pure BlockFetcherConfig {..}

configT :: TomlCodec Config
configT = do
  cfgDbConnectionString <- Toml.byteString "dbConnectionString" .= cfgDbConnectionString
  cfgServerPort <- Toml.int "server.port" .= cfgServerPort
  cfgServerControlApiToken <-
    Toml.diwrap (Toml.string "server.controlApiToken") .= cfgServerControlApiToken
  cfgOgmiosAddress <- Toml.string "ogmios.address" .= cfgOgmiosAddress
  cfgOgmiosPort <- Toml.int "ogmios.port" .= cfgOgmiosPort
  cfgFetcher <- withFetcherT .= cfgFetcher
  pure Config {..}

loadConfig :: MonadIO m => OldConfigOption -> m Config
loadConfig OldConfigOption {config} = do
  tomlRes <- Toml.decodeFileEither configT config
  case tomlRes of
    Left errs -> error $ toString $ Toml.prettyTomlDecodeErrors errs
    Right conf -> return conf
