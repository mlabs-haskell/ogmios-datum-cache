{-# LANGUAGE DuplicateRecordFields #-}
module Api.Types where

import Servant.API.Generic (Generic)
import Data.Vector (Vector)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Data.Set (Set)

import qualified PlutusData

data GetDatumByHashResponse = GetDatumByHashResponse PlutusData.Data
  deriving stock Generic
  deriving anyclass ToJSON

data GetDatumsByHashesRequest = GetDatumsByHashesRequest
  { hashes :: [Text]
  }
  deriving stock Generic
  deriving anyclass FromJSON

data GetDatumsByHashesDatum = GetDatumsByHashesDatum
  { hash :: Text
  , value :: PlutusData.Data
  }
  deriving stock Generic
  deriving anyclass ToJSON

data GetDatumsByHashesResponse = GetDatumsByHashesResponse
  { datums :: Vector GetDatumsByHashesDatum
  }
  deriving stock Generic
  deriving anyclass ToJSON

data AddDatumHashesRequest = AddDatumHashesRequest
  { hashes :: [Text]
  }
  deriving stock Generic
  deriving anyclass FromJSON

data AddDatumHashesResponse = AddDatumHashesResponse
  { message :: Text
  }
  deriving stock Generic
  deriving anyclass ToJSON

data RemoveDatumHashesRequest = RemoveDatumHashesRequest
  { hashes :: [Text]
  }
  deriving stock Generic
  deriving anyclass FromJSON

data RemoveDatumHashesResponse = RemoveDatumHashesResponse
  { message :: Text
  }
  deriving stock Generic
  deriving anyclass ToJSON

data SetDatumHashesRequest = SetDatumHashesRequest
  { hashes :: [Text]
  }
  deriving stock Generic
  deriving anyclass FromJSON

data SetDatumHashesResponse = SetDatumHashesResponse
  { message :: Text
  }
  deriving stock Generic
  deriving anyclass ToJSON

data GetDatumHashesResponse = GetDatumHashesResponse
  { hashes :: Set Text
  }
  deriving stock Generic
  deriving anyclass ToJSON

data StartBlockFetchingRequest = StartBlockFetchingRequest
  { slot :: Integer
  , id :: Text
  }
  deriving stock Generic
  deriving anyclass FromJSON

data StartBlockFetchingResponse = StartBlockFetchingResponse
  { message :: Text
  }
  deriving stock Generic
  deriving anyclass ToJSON

data CancelBlockFetchingResponse = CancelBlockFetchingResponse
  { message :: Text
  }
  deriving stock Generic
  deriving anyclass ToJSON
