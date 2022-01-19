{-# LANGUAGE InstanceSigs #-}
module App.Env where

import qualified Hasql.Connection as Hasql
import GHC.Generics (Generic)

import qualified Colog
import Colog (HasLog, LogAction, Message)

import App.RequestedDatumHashes
import App.FirstFetchBlock

data Env m = Env
  { envRequestedDatumHashes :: RequestedDatumHashes
  , envSaveAllDatums :: Bool
  , envFirstFetchBlock :: FirstFetchBlock
  -- TODO: (?) pool
  , envDbConnection :: Hasql.Connection
  , envLogAction :: Colog.LogAction m Colog.Message
  }
  deriving stock Generic

instance HasLog (Env m) Message m where
  getLogAction :: Env m -> LogAction m Message
  getLogAction = envLogAction
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction m Message -> Env m -> Env m
  setLogAction newLogAction env = env { envLogAction = newLogAction }
  {-# INLINE setLogAction #-}
