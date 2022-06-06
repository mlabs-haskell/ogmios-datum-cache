module App.Env (Env (..)) where

import Control.Concurrent.MVar (MVar)
import Control.Monad.Reader.Has (Has)
import GHC.Generics (Generic)
import Hasql.Connection qualified as Hasql

import Block.Fetch (BlockFetcherEnv, OgmiosInfo)

data Env = Env
  { -- TODO: Switch to pool of connections
    envDbConnection :: Hasql.Connection
  , envOgmiosInfo :: OgmiosInfo
  , envBlockFetcherEnv :: MVar BlockFetcherEnv
  }
  deriving stock (Generic)
  deriving anyclass (Has Hasql.Connection, Has OgmiosInfo, Has (MVar BlockFetcherEnv))
