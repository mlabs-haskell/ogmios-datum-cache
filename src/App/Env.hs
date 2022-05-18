module App.Env (Env (..)) where

import Control.Monad.Reader.Has (Has)
import GHC.Generics (Generic)
import Hasql.Connection qualified as Hasql

import Block.Fetch

data Env = Env
  { -- TODO: Switch to pool of connections
    envDbConnection :: Hasql.Connection
  , envOgmiosInfo :: OgmiosInfo
  , envOgmiosWorker :: OgmiosWorkerMVar
  , envControlApiToken :: ControlApiToken
  }
  deriving stock (Generic)
  deriving anyclass
    ( Has Hasql.Connection
    , Has OgmiosWorkerMVar
    , Has OgmiosInfo
    , Has ControlApiToken
    )
