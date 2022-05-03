module App.Env (Env (..)) where

import Control.Monad.Reader.Has (Has)
import GHC.Generics (Generic)
import Hasql.Connection qualified as Hasql

import Block.Fetch
import Block.Filter (DatumFilter)

data Env = Env
    { envDatumFilter :: DatumFilter
    , -- TODO: Switch to pool of connections
      envDbConnection :: Hasql.Connection
    , envOgmiosInfo :: OgmiosInfo
    , envOgmiosWorker :: OgmiosWorkerMVar
    }
    deriving stock (Generic)
    deriving anyclass (Has DatumFilter, Has Hasql.Connection, Has OgmiosWorkerMVar, Has OgmiosInfo)
