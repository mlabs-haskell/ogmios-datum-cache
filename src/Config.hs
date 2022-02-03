module Config (loadConfig, Config (..)) where

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Toml (TomlCodec, (.=))
import Toml qualified

data Config = Config
    { cfgDbConnectionString :: ByteString
    , cfgSaveAllDatums :: Bool
    , cfgServerPort :: Int
    , cfgOgmiosAddress :: String
    , cfgOgmiosPort :: Int
    , cfgFirstFetchBlockSlot :: Integer
    , cfgFirstFetchBlockId :: Text
    }

configT :: TomlCodec Config
configT =
    Config
        <$> Toml.byteString "dbConnectionString" .= cfgDbConnectionString
        <*> Toml.bool "saveAllDatums" .= cfgSaveAllDatums
        <*> Toml.int "server.port" .= cfgServerPort
        <*> Toml.string "ogmios.address" .= cfgOgmiosAddress
        <*> Toml.int "ogmios.port" .= cfgOgmiosPort
        <*> Toml.integer "firstFetchBlock.slot" .= cfgFirstFetchBlockSlot
        <*> Toml.text "firstFetchBlock.id" .= cfgFirstFetchBlockId

loadConfig :: MonadIO m => m Config
loadConfig = Toml.decodeFile configT "config.toml"
