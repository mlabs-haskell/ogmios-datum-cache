module App.Env (
  Env (..),
  ControlApiToken (..),
) where

import Control.Concurrent.MVar (MVar)
import Control.Monad.Reader.Has (Has)
import Data.String (IsString (fromString))
import GHC.Generics (Generic)
import Hasql.Connection qualified as Hasql

import Block.Fetch (BlockFetcherEnv, BlockProcessorEnv)

data Env = Env
  { -- TODO: Switch to pool of connections
    envDbConnection :: Hasql.Connection
  , envBlockFetcherEnv :: MVar BlockFetcherEnv
  , envBlockProcessorEnv :: BlockProcessorEnv
  , envControlApiToken :: ControlApiToken
  }
  deriving stock (Generic)
  deriving anyclass
    ( Has Hasql.Connection
    , Has (MVar BlockFetcherEnv)
    , Has BlockProcessorEnv
    , Has ControlApiToken
    )

newtype ControlApiToken = ControlApiToken {unControlApiToken :: String}
  deriving stock (Eq, Show)

instance IsString ControlApiToken where
  fromString = ControlApiToken
