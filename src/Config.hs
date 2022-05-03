module Config (loadConfig, Config (..)) where

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Toml (TomlCodec, dimap, (.=))
import Toml qualified

data Config = Config
    { cfgDbConnectionString :: ByteString
    , cfgDatumFilterPath :: Maybe FilePath
    , cfgServerPort :: Int
    , cfgOgmiosAddress :: String
    , cfgOgmiosPort :: Int
    , cfgFirstFetchBlockSlot :: Int64
    , cfgFirstFetchBlockId :: Text
    , cfgAutoStartFetcher :: Bool
    , cfgStartFromLastBlock :: Bool
    }
    deriving stock (Show)

withDefault :: a -> TomlCodec a -> TomlCodec a
withDefault d c = dimap pure (fromMaybe d) (Toml.dioptional c)

int64 :: Toml.Key -> TomlCodec Int64
int64 k = dimap fromIntegral fromIntegral (Toml.integer k)

configT :: TomlCodec Config
configT =
    Config
        <$> Toml.byteString "dbConnectionString" .= cfgDbConnectionString
        <*> Toml.dioptional (Toml.string "datumFilterPath") .= cfgDatumFilterPath
        <*> Toml.int "server.port" .= cfgServerPort
        <*> Toml.string "ogmios.address" .= cfgOgmiosAddress
        <*> Toml.int "ogmios.port" .= cfgOgmiosPort
        <*> int64 "firstFetchBlock.slot" .= cfgFirstFetchBlockSlot
        <*> Toml.text "firstFetchBlock.id" .= cfgFirstFetchBlockId
        <*> withDefault False (Toml.bool "blockFetcher.autoStart") .= cfgAutoStartFetcher
        <*> withDefault False (Toml.bool "blockFetcher.startFromLast") .= cfgStartFromLastBlock

loadConfig :: MonadIO m => m Config
loadConfig = Toml.decodeFile configT "config.toml"
