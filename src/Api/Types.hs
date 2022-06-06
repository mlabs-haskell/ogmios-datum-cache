module Api.Types (
  GetDatumByHashResponse (..),
  GetDatumsByHashesRequest (..),
  GetDatumsByHashesDatum (..),
  GetDatumsByHashesResponse (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Vector (Vector)
import Servant.API.Generic (Generic)

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
