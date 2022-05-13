{-# LANGUAGE DuplicateRecordFields #-}

module Api.Types (
  GetDatumByHashResponse (..),
  GetDatumsByHashesRequest (..),
  GetDatumsByHashesDatum (..),
  GetDatumsByHashesResponse (..),
  StartBlockFetchingRequest (..),
  StartBlockFetchingResponse (..),
  CancelBlockFetchingResponse (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import Servant.API.Generic (Generic)

import Block.Filter (DatumFilter)
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

data StartBlockFetchingRequest = StartBlockFetchingRequest
  { slot :: Int64
  , id :: Text
  , datumFilter :: Maybe DatumFilter
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
