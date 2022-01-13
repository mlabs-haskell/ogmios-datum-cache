module App.Env where

import qualified Hasql.Connection as Hasql
import GHC.Generics (Generic)

import App.RequestedDatumHashes

data Env = Env
  { envRequestedDatumHashes :: RequestedDatumHashes
  , envSaveAllDatums :: Bool
  -- TODO: (?) pool
  , envDbConnection :: Hasql.Connection
  }
  deriving stock Generic
