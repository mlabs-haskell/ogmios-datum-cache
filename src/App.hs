module App (App (..)) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Reader (MonadReader, ReaderT)

import App.Env (Env)

newtype App a = App {unApp :: ReaderT Env (LoggingT IO) a}
    deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadThrow, MonadUnliftIO, MonadLogger)
