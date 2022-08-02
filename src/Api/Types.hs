module Api.Types (
  GetDatumByHashResponse (..),
  GetDatumsByHashesRequest (..),
  GetDatumsByHashesResponse (..),
  SetStartingBlockRequest (..),
  SetDatumFilterRequest (..),
  ControlApiAuthData (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Servant.API.Generic (Generic)

import Block.Filter (DatumFilter)
import Block.Types (StartingBlock)
import DataHash (DataHash)
import Database (DatabaseError)
import PlutusData qualified

newtype GetDatumByHashResponse = GetDatumByHashResponse PlutusData.Data
  deriving stock (Generic)
  deriving anyclass (ToJSON)

newtype GetDatumsByHashesRequest = GetDatumsByHashesRequest
  { hashes :: [DataHash]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

newtype GetDatumsByHashesResponse = GetDatumsByHashesResponse
  { datums :: Map DataHash (Either DatabaseError PlutusData.Data)
  }
  deriving stock (Generic,Show)
  deriving anyclass (ToJSON)

newtype SetStartingBlockRequest = SetStartingBlockRequest
  { startingBlock :: StartingBlock
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data ControlApiAuthData = ControlApiAuthData

newtype SetDatumFilterRequest = SetDatumFilterRequest
  { datumFilter :: DatumFilter
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)
