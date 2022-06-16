module Api.Types (
  GetDatumByHashResponse (..),
  GetDatumsByHashesRequest (..),
  GetDatumsByHashesDatum (..),
  GetDatumsByHashesResponse (..),
  SetStartingBlockRequest (..),
  SetDatumFilterRequest (..),
  ControlApiAuthData (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Vector (Vector)
import Servant.API.Generic (Generic)

import Block.Filter (DatumFilter)
import Block.Types (BlockInfo)
import PlutusData qualified

newtype GetDatumByHashResponse = GetDatumByHashResponse PlutusData.Data
  deriving stock (Generic)
  deriving anyclass (ToJSON)

newtype GetDatumsByHashesRequest = GetDatumsByHashesRequest
  { hashes :: [Text]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data GetDatumsByHashesDatum = GetDatumsByHashesDatum
  { hash :: Text
  , value :: PlutusData.Data
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

newtype GetDatumsByHashesResponse = GetDatumsByHashesResponse
  { datums :: Vector GetDatumsByHashesDatum
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

newtype SetStartingBlockRequest = SetStartingBlockRequest
  { startingBlock :: BlockInfo
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data ControlApiAuthData = ControlApiAuthData

newtype SetDatumFilterRequest = SetDatumFilterRequest
  { datumFilter :: DatumFilter
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)
