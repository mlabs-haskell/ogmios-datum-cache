module App ( App (..) ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT)

import App.Env

newtype App a = App { unApp :: ReaderT Env IO a }
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadIO)
