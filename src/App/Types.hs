module App.Types (App (..)) where

import Control.Monad.Catch (Exception, MonadThrow, throwM, try)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Reader (MonadReader, ReaderT)
import Hasql.Connection qualified as Connection
import Hasql.Connection qualified as Hasql

import App.Env (ControlApiToken (ControlApiToken), Env (..))
import Block.Fetch (
  OgmiosInfo (OgmiosInfo),
  createStoppedFetcher,
  startBlockFetcher,
 )
import Config (BlockFetcherConfig (BlockFetcherConfig), Config (..), loadConfig)

newtype App a = App {unApp :: ReaderT Env (LoggingT IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadThrow, MonadUnliftIO, MonadLogger)
