module App.Env (Env (..)) where

import Control.Concurrent.MVar (MVar)
import Control.Monad.Reader.Has (Has)
import GHC.Generics (Generic)
import Hasql.Connection qualified as Hasql

import Block.Fetch (BlockFetcherEnv, BlockProcessorEnv)

data Env = Env
  { -- TODO: Switch to pool of connections
    envDbConnection :: Hasql.Connection
  , envBlockFetcherEnv :: MVar BlockFetcherEnv
  , envBlockProcessorEnv :: BlockProcessorEnv
  }
  deriving stock (Generic)
  deriving anyclass (Has Hasql.Connection, Has (MVar BlockFetcherEnv), Has BlockProcessorEnv)
