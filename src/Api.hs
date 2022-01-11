module Api where

import Servant.API.Generic (Generic, ToServantApi, genericApi, (:-))
import Servant
import Data.Text (Text)

import Api.Types

data DatumApi route = DatumApi
  { getDatumByHash ::
      route
        :- "datum"
        :> Summary "Get datum value by sha256 hash"
        :> Capture "hash" Text
        :> Get '[JSON] GetDatumByHashResponse
  , getDatumsByHashes ::
      route
        :- "datums"
        :> Summary "Get several datum values by their sha256 hashes"
        -- TODO: change to JSON request
        :> ReqBody '[JSON] GetDatumsByHashesRequest
        :> Get '[JSON] GetDatumsByHashesResponse
  }
  deriving stock Generic

data ControlApi route = ControlApi
  { addDatumHashes ::
      route
        :- "add_hashes"
        :> Summary "Add a set of additional datum hashes for fetching"
        :> ReqBody '[JSON] AddDatumHashesRequest
        :> Post '[JSON] AddDatumHashesResponse
  , removeDatumHashes ::
      route
        :- "remove_hashes"
        :> Summary "Don't fetch specified datum hashes"
        :> ReqBody '[JSON] RemoveDatumHashesRequest
        :> Post '[JSON] RemoveDatumHashesResponse
  , setDatumHashes ::
      route
        :- "set_hashes"
        :> Summary "Set a set of datum hashes for fetching"
        :> ReqBody '[JSON] SetDatumHashesRequest
        :> Post '[JSON] SetDatumHashesResponse
  }
  deriving stock Generic

data Routes route = Routes
  { datumRoutes :: route :- ToServantApi DatumApi
  , controlRoutes :: route :- "control" :> ToServantApi ControlApi
  }
  deriving stock Generic

datumCacheApi :: Proxy (ToServantApi Routes)
datumCacheApi = genericApi (Proxy :: Proxy Routes)
