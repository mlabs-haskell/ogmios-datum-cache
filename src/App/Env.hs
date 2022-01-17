module App.Env where

import qualified Hasql.Connection as Hasql
import GHC.Generics (Generic)

import App.RequestedDatumHashes
import App.FirstFetchBlock

data Env = Env
  { envRequestedDatumHashes :: RequestedDatumHashes
  , envSaveAllDatums :: Bool
  , envFirstFetchBlock :: FirstFetchBlock
  -- TODO: (?) pool
  , envDbConnection :: Hasql.Connection
  }
  deriving stock Generic
