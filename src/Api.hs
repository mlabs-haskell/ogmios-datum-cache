module Api (
  Routes (..),
  datumCacheApi,
  datumCacheContext,
  DatumApi (..),
  ControlApi (..),
  WebSocketApi (..),
  ControlApiAuthData (ControlApiAuthData),
) where

import Data.Text (Text)
import Servant (Capture, Get, JSON, Post, Proxy (Proxy), ReqBody, Summary, (:>))
import Servant.API.BasicAuth (BasicAuth)
import Servant.API.Generic (Generic, ToServantApi, genericApi, (:-))
import Servant.API.WebSocket (WebSocket)
import Servant.Server (BasicAuthCheck)

import Api.Types (
  CancelBlockFetchingRequest,
  CancelBlockFetchingResponse,
  GetDatumByHashResponse,
  GetDatumsByHashesRequest,
  GetDatumsByHashesResponse,
  StartBlockFetchingRequest,
  StartBlockFetchingResponse,
 )
import Block.Types (BlockInfo)

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
  , getLastBlock ::
      route
        :- "block"
        :> Summary "Get latest processed block"
        :> Get '[JSON] BlockInfo
  , getHealthcheck ::
      route
        :- "healthcheck"
        :> Summary "Succeeds if service is alive, otherwise not"
        :> Get '[JSON] ()
  }
  deriving stock (Generic)

data ControlApi route = ControlApi
  { startBlockFetching ::
      route
        :- "fetch_blocks"
        :> Summary "Start a block fetcher starting with the specified block"
        :> ReqBody '[JSON] StartBlockFetchingRequest
        :> Post '[JSON] StartBlockFetchingResponse
  , cancelBlockFetchingWithBody ::
      route
        :- "cancel_fetch_blocks"
        :> Summary "Stop a block fetcher"
        :> ReqBody '[JSON] CancelBlockFetchingRequest
        :> Post '[JSON] CancelBlockFetchingResponse
  , cancelBlockFetching ::
      route
        :- "cancel_fetch_blocks"
        :> Summary "Stop a block fetcher"
        :> Post '[JSON] CancelBlockFetchingResponse
  }
  deriving stock (Generic)

newtype WebSocketApi route = WebSocketApi
  { websocketApi ::
      route
        :- WebSocket
  }
  deriving stock (Generic)

data ControlApiAuthData = ControlApiAuthData

data Routes route = Routes
  { datumRoutes :: route :- ToServantApi DatumApi
  , controlRoutes ::
      route
        :- "control"
        :> BasicAuth "control-api-realm" ControlApiAuthData
        :> ToServantApi ControlApi
  , websocketRoutes :: route :- "ws" :> ToServantApi WebSocketApi
  }
  deriving stock (Generic)

datumCacheContext :: Proxy '[BasicAuthCheck ControlApiAuthData]
datumCacheContext = Proxy

datumCacheApi :: Proxy (ToServantApi Routes)
datumCacheApi = genericApi (Proxy :: Proxy Routes)
