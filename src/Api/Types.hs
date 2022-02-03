{-# LANGUAGE DuplicateRecordFields #-}

module Api.Types (
    GetDatumByHashResponse (..),
    GetDatumsByHashesRequest (..),
    GetDatumsByHashesDatum (..),
    GetDatumsByHashesResponse (..),
    AddDatumHashesRequest (..),
    AddDatumHashesResponse (..),
    RemoveDatumHashesRequest (..),
    RemoveDatumHashesResponse (..),
    SetDatumHashesRequest (..),
    SetDatumHashesResponse (..),
    GetDatumHashesResponse (..),
    StartBlockFetchingRequest (..),
    StartBlockFetchingResponse (..),
    CancelBlockFetchingResponse (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Set (Set)
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

newtype AddDatumHashesRequest = AddDatumHashesRequest
    { hashes :: [Text]
    }
    deriving stock (Generic)
    deriving anyclass (FromJSON)

newtype AddDatumHashesResponse = AddDatumHashesResponse
    { message :: Text
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON)

newtype RemoveDatumHashesRequest = RemoveDatumHashesRequest
    { hashes :: [Text]
    }
    deriving stock (Generic)
    deriving anyclass (FromJSON)

newtype RemoveDatumHashesResponse = RemoveDatumHashesResponse
    { message :: Text
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON)

newtype SetDatumHashesRequest = SetDatumHashesRequest
    { hashes :: [Text]
    }
    deriving stock (Generic)
    deriving anyclass (FromJSON)

newtype SetDatumHashesResponse = SetDatumHashesResponse
    { message :: Text
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON)

newtype GetDatumHashesResponse = GetDatumHashesResponse
    { hashes :: Set Text
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON)

data StartBlockFetchingRequest = StartBlockFetchingRequest
    { slot :: Integer
    , id :: Text
    }
    deriving stock (Generic)
    deriving anyclass (FromJSON)

newtype StartBlockFetchingResponse = StartBlockFetchingResponse
    { message :: Text
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON)

newtype CancelBlockFetchingResponse = CancelBlockFetchingResponse
    { message :: Text
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON)
