module Config (loadConfig, Config (..)) where

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
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
    , cfgFirstFetchBlockSlot :: Integer
    , cfgFirstFetchBlockId :: Text
    , cfgAutoStartFetcher :: Bool
    }

withDefault :: a -> TomlCodec a -> TomlCodec a
withDefault d c = dimap pure (fromMaybe d) (Toml.dioptional c)

configT :: TomlCodec Config
configT =
    Config
        <$> Toml.byteString "dbConnectionString" .= cfgDbConnectionString
        <*> Toml.dioptional (Toml.string "datumFilterPath") .= cfgDatumFilterPath
        <*> Toml.int "server.port" .= cfgServerPort
        <*> Toml.string "ogmios.address" .= cfgOgmiosAddress
        <*> Toml.int "ogmios.port" .= cfgOgmiosPort
        <*> Toml.integer "firstFetchBlock.slot" .= cfgFirstFetchBlockSlot
        <*> Toml.text "firstFetchBlock.id" .= cfgFirstFetchBlockId
        <*> withDefault False (Toml.bool "blockFetcher.autoStart") .= cfgAutoStartFetcher

loadConfig :: MonadIO m => m Config
loadConfig = Toml.decodeFile configT "config.toml"
