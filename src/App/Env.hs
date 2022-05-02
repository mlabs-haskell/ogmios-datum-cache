{-# LANGUAGE InstanceSigs #-}

module App.Env (Env (..)) where

import Colog (HasLog, LogAction, Message)
import Colog qualified
import Control.Concurrent.MVar (MVar)
import GHC.Generics (Generic)
import Hasql.Connection qualified as Hasql
import UnliftIO.Async (Async)

import App.FirstFetchBlock (FirstFetchBlock)
import App.RequestedDatumHashes (RequestedDatumHashes)
import Block.Filter (DatumFilter)

data Env m = Env
    { envRequestedDatumHashes :: RequestedDatumHashes
    , envDatumFilter :: DatumFilter
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
