module Api (
  Routes (..),
  txCacheApi,
  CacheApi (..),
  ControlApi (..),
  WebSocketApi (..),
) where

import Data.Text (Text)
import Servant (Capture, Get, JSON, Post, Proxy (Proxy), ReqBody, Summary, (:>))
import Servant.API.Generic (Generic, ToServantApi, genericApi, (:-))
import Servant.API.WebSocket (WebSocket)

import Api.Types (
  CancelBlockFetchingResponse,
  GetDatumByHashResponse,
  GetDatumsByHashesRequest,
  GetDatumsByHashesResponse,
  {-
  GetTransactionByIdResponse,
  GetTransactionsByIdsRequest,
  GetTransactionsByIdsResponse
  -}
  StartBlockFetchingRequest,
  StartBlockFetchingResponse,
 )
import Block.Types (BlockInfo)

data CacheApi route = CacheApi
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

data Routes route = Routes
  { cacheRoutes :: route :- ToServantApi CacheApi
  , controlRoutes :: route :- "control" :> ToServantApi ControlApi
  , websocketRoutes :: route :- "ws" :> ToServantApi WebSocketApi
  }
  deriving stock (Generic)

txCacheApi :: Proxy (ToServantApi Routes)
txCacheApi = genericApi (Proxy :: Proxy Routes)
