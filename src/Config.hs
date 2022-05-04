{-# LANGUAGE ApplicativeDo #-}

module Config (loadConfig, Config (..), BlockFetcherConfig (..)) where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Toml (TomlCodec, dimap, (.=))
import Toml qualified

import Block.Types (BlockInfo (BlockInfo), blockId, blockSlot)

data BlockFetcherConfig = BlockFetcherConfig
    { cfgFetcherBlock :: BlockInfo
    , cfgFetcherFilterJson :: LBS.ByteString
    , cfgFetcherUseLatest :: Bool
    }
    deriving stock (Show)

data Config = Config
    { cfgDbConnectionString :: ByteString
    , cfgServerPort :: Int
    , cfgOgmiosAddress :: String
    , cfgOgmiosPort :: Int
    , cfgFetcher :: Maybe BlockFetcherConfig
    }
    deriving stock (Show)

withDefault :: a -> TomlCodec a -> TomlCodec a
withDefault d c = dimap pure (fromMaybe d) (Toml.dioptional c)

int64 :: Toml.Key -> TomlCodec Int64
int64 k = dimap fromIntegral fromIntegral (Toml.integer k)

matchTrue :: Toml.Value t -> Either Toml.MatchError Bool
matchTrue (Toml.Bool True) = Right True
matchTrue value = Toml.mkMatchError Toml.TBool value

true :: Toml.Key -> TomlCodec Bool
true = Toml.match $ Toml.mkAnyValueBiMap matchTrue Toml.Bool

matchFalse :: Toml.Value t -> Either Toml.MatchError Bool
matchFalse (Toml.Bool False) = Right False
matchFalse value = Toml.mkMatchError Toml.TBool value

false :: Toml.Key -> TomlCodec Bool
false = Toml.match $ Toml.mkAnyValueBiMap matchFalse Toml.Bool

-- latestBlockT :: Toml.Codec Config FirstBlockInfo
-- latestBlockT = do
--   withDefault False (true "blockFetcher.startFromLast") .= const True
--   pure LatestBlock

-- concreateBlockT :: Toml.Codec Config FirstBlockInfo
-- concreateBlockT = do
--   let getConcrete (ConcreateBlock block) = Just block
--       getConcrete _ = Nothing
--   withDefault False (false "blockFetcher.startFromLast") .= const False
--   blockSlot' <- int64 "firstFetchBlock.slot"
--     .= (maybe 0 (maybe 0 blockSlot . getConcrete . cfgFetcherBlock) . cfgFetcher)
--   blockId' <- Toml.text "firstFetchBlock.id"
--     .= (maybe "" (maybe "" blockId . getConcrete . cfgFetcherBlock) . cfgFetcher)
--   pure $ ConcreateBlock $ BlockInfo blockSlot' blockId'

noFetcherT :: Toml.Codec Config (Maybe BlockFetcherConfig)
noFetcherT = do
    withDefault False (false "blockFetcher.autoStart") .= const False
    pure Nothing

blockInfoT :: Toml.Codec Config BlockInfo
blockInfoT = do
    blockSlot' <-
        int64 "firstFetchBlock.slot"
            .= maybe 0 (blockSlot . cfgFetcherBlock) . cfgFetcher
    blockId' <-
        Toml.text "firstFetchBlock.id"
            .= maybe "" (blockId . cfgFetcherBlock) . cfgFetcher
    pure $ BlockInfo blockSlot' blockId'

withFetcherT :: Toml.Codec Config (Maybe BlockFetcherConfig)
withFetcherT = do
    withDefault False (true "blockFetcher.autoStart") .= const True
    cfgFetcherFilterJson <-
        withDefault "{ \"const\" = true  }" (Toml.lazyByteString "")
            .= (maybe "" cfgFetcherFilterJson . cfgFetcher)
    cfgFetcherBlock <- blockInfoT
    cfgFetcherUseLatest <-
        withDefault False (Toml.bool "")
            .= maybe False cfgFetcherUseLatest . cfgFetcher
    pure $ Just BlockFetcherConfig{..}

configT :: TomlCodec Config
configT = do
    cfgDbConnectionString <- Toml.byteString "dbConnectionString" .= cfgDbConnectionString
    cfgServerPort <- Toml.int "server.port" .= cfgServerPort
    cfgOgmiosAddress <- Toml.string "ogmios.address" .= cfgOgmiosAddress
    cfgOgmiosPort <- Toml.int "ogmios.port" .= cfgOgmiosPort
    cfgFetcher <- noFetcherT <|> withFetcherT

    pure Config{..}

loadConfig :: MonadIO m => m Config
loadConfig = Toml.decodeFile configT "config.toml"
