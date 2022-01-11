module App.Env where

import qualified Hasql.Connection as Hasql
import Data.Text (Text)
import Data.Set (Set)
import GHC.Generics (Generic)
import Control.Concurrent.MVar (MVar)

type DatumHashes = Set Text

data Env = Env
  { envRequestedDatumHashes :: MVar DatumHashes
  -- TODO: (?) pool
  , envDbConnection :: Hasql.Connection
  }
  deriving stock Generic
