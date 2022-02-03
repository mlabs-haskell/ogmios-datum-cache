{-# LANGUAGE InstanceSigs #-}

module App.Env where

import GHC.Generics (Generic)
import Hasql.Connection qualified as Hasql

import Colog (HasLog, LogAction, Message)
import Colog qualified
import Control.Concurrent.MVar (MVar)
import UnliftIO.Async (Async)

import App.FirstFetchBlock
import App.RequestedDatumHashes

data Env m = Env
    { envRequestedDatumHashes :: RequestedDatumHashes
    , envSaveAllDatums :: Bool
    , envFirstFetchBlock :: FirstFetchBlock
    , -- TODO: (?) pool
      envDbConnection :: Hasql.Connection
    , envLogAction :: Colog.LogAction m Colog.Message
    , envOgmiosAddress :: String
    , envOgmiosPort :: Int
    , envOgmiosWorker :: MVar (Async ())
    }
    deriving stock (Generic)

instance HasLog (Env m) Message m where
    getLogAction :: Env m -> LogAction m Message
    getLogAction = envLogAction
    {-# INLINE getLogAction #-}

    setLogAction :: LogAction m Message -> Env m -> Env m
    setLogAction newLogAction env = env{envLogAction = newLogAction}
    {-# INLINE setLogAction #-}
