module App.Env (
  Env (..),
  ControlApiToken (..),
  AuthToken,
  checkControlApiToken,
) where

import Control.Monad.Reader.Has (Has)
import Data.Maybe (isNothing)
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

type AuthToken = Maybe String

newtype ControlApiToken = ControlApiToken {unControlApiToken :: AuthToken}
  deriving stock (Eq, Show)

checkControlApiToken :: AuthToken -> AuthToken -> Bool
checkControlApiToken expect actual = isNothing expect || expect == actual
