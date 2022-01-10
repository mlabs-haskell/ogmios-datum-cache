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

data Routes route = Routes
  { datumRoutes :: route :- ToServantApi DatumApi
  }
  deriving stock Generic

datumApi :: Proxy (ToServantApi Routes)
datumApi = genericApi (Proxy :: Proxy Routes)
