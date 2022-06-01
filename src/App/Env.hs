module App.Env (
  Env (..),
  ControlApiToken (..),
) where

import Control.Monad.Reader.Has (Has)
import Data.String (IsString (fromString))
import GHC.Generics (Generic)
import Hasql.Connection qualified as Hasql

import Block.Fetch (OgmiosInfo, OgmiosWorkerMVar)

data Env = Env
  { -- TODO: Switch to pool of connections
    envDbConnection :: Hasql.Connection
  , envOgmiosInfo :: OgmiosInfo
  , envOgmiosWorker :: OgmiosWorkerMVar
  , envControlApiToken :: Maybe ControlApiToken
  }
  deriving stock (Generic)
  deriving anyclass
    ( Has Hasql.Connection
    , Has OgmiosWorkerMVar
    , Has OgmiosInfo
    , Has (Maybe ControlApiToken)
    )

newtype ControlApiToken = ControlApiToken {unControlApiToken :: String}
  deriving stock (Eq, Show)

instance IsString ControlApiToken where
  fromString = ControlApiToken
