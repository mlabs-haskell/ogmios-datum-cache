module Api.Handler where

import Servant
import Servant.API.Generic (ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)
import Data.Text (Text)

import qualified PlutusData
import Api

datumServiceHandlers :: Routes (AsServerT Handler)
datumServiceHandlers = Routes{..}
  where
    datumRoutes :: ToServant DatumApi (AsServerT Handler)
    datumRoutes = genericServerT DatumApi{..}

    getDatumByHash :: Text -> Handler GetDatumByHashResponse
    getDatumByHash hash = do
      pure $ GetDatumByHashResponse (PlutusData.I 100)

    getDatumsByHashes :: Text -> Handler GetDatumsByHashesResponse
    getDatumsByHashes hashes = do
      pure $ GetDatumsByHashesResponse [PlutusData.I 100, PlutusData.I 200]
