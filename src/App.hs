module App (App (..)) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import Servant.Server (ServerError)

import App.Env

newtype App a = App {unApp :: ReaderT (Env App) IO a}
    deriving newtype (Functor, Applicative, Monad, MonadReader (Env App), MonadIO, MonadThrow, MonadUnliftIO)
