module Api (
  Routes (..),
  datumCacheApi,
  datumCacheContext,
  DatumApi (..),
  ControlApi (..),
  WebSocketApi (..),
) where

import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Servant (Capture, Get, JSON, Post, Proxy (Proxy), ReqBody, Summary, (:>))
import Servant.API.BasicAuth (BasicAuth)
import Servant.API.Generic (Generic, ToServantApi, genericApi, (:-))
import Servant.API.WebSocket (WebSocket)
import Servant.Server (BasicAuthCheck)

import Api.Types (
  ControlApiAuthData,
  GetDatumByHashResponse,
  GetDatumsByHashesRequest,
  GetDatumsByHashesResponse,
  SetDatumFilterRequest,
  SetStartingBlockRequest,
 )
import Block.Types (BlockInfo, CursorPoint)

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
  , getTx ::
      route
        :- "tx"
        :> Summary "Get transaction by txId"
        :> Capture "txId" Text
        :> Get '[JSON] Aeson.Value
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
  { setStartingBlock ::
      route
        :- "startingBlock"
        :> Summary "Set starting block for block fetcher"
        :> ReqBody '[JSON] SetStartingBlockRequest
        :> Post '[JSON] CursorPoint
  , setDatumFilter ::
      route
        :- "datumFilter"
        :> Summary "Set datum filter"
        :> ReqBody '[JSON] SetDatumFilterRequest
        :> Post '[JSON] ()
  }
  deriving stock (Generic)

newtype WebSocketApi route = WebSocketApi
  { websocketApi ::
      route
        :- WebSocket
  }
  deriving stock (Generic)

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
