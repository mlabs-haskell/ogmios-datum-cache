module Api (
    Routes (..),
    datumCacheApi,
    DatumApi (..),
    ControlApi (..),
    WebSocketApi (..),
) where

import Data.Text (Text)
import Servant (Capture, Get, JSON, Post, Proxy (..), ReqBody, Summary, (:>))
import Servant.API.Generic (Generic, ToServantApi, genericApi, (:-))
import Servant.API.WebSocket (WebSocket)

import Api.Types (
    AddDatumHashesRequest,
    AddDatumHashesResponse,
    CancelBlockFetchingResponse,
    GetDatumByHashResponse,
    GetDatumHashesResponse,
    GetDatumsByHashesDatum,
    GetDatumsByHashesRequest,
    GetDatumsByHashesResponse,
    RemoveDatumHashesRequest,
    RemoveDatumHashesResponse,
    SetDatumHashesRequest,
    SetDatumHashesResponse,
    StartBlockFetchingRequest,
    StartBlockFetchingResponse,
 )

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
    deriving stock (Generic)

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
    , getDatumHashes ::
        route
            :- "get_hashes"
            :> Summary "Get a set of datum hashes for fetching"
            :> Get '[JSON] GetDatumHashesResponse
    , startBlockFetching ::
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
    { datumRoutes :: route :- ToServantApi DatumApi
    , controlRoutes :: route :- "control" :> ToServantApi ControlApi
    , websocketRoutes :: route :- "ws" :> ToServantApi WebSocketApi
    }
    deriving stock (Generic)

datumCacheApi :: Proxy (ToServantApi Routes)
datumCacheApi = genericApi (Proxy :: Proxy Routes)
