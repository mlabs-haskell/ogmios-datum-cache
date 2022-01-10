{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Network.Socket (withSocketsDo)
import qualified Hasql.Connection as Connection
import qualified Hasql.Connection as Hasql
import qualified Network.WebSockets  as WS
import Network.Wai.Handler.Warp qualified as W
import Network.Wai.Logger (withStdoutLogger)
import Servant.Server.Generic   (genericServerT)
import Servant.Server (Application, Handler (..), serve, ServerT)
import Servant.API.Generic (ToServantApi)
import Control.Concurrent (forkIO)

import qualified PlutusData
import Api (Routes, datumApi)
import Api.Handler (datumServiceHandlers)
import Database (datumInsertSession, getDatumSession, Datum (..))
import Block.Fetch (wsApp)

appService :: Hasql.Connection -> Application
appService pgConn = serve datumApi appServer
  where
    appServer :: ServerT (ToServantApi Routes) Handler
    appServer = genericServerT (datumServiceHandlers pgConn)

main :: IO ()
main = do
  -- CREATE TABLE datums (hash text, value bytea);
  -- CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS datums_hash_index ON datums (hash);
  Right pgConn <- Connection.acquire connSettings

  -- Right datumRes <- Session.run (getDatumSession "ca3b51aec7831376f5cbbd96d3370f821b4593462edd4ea42e31edfd7fb03485") pgConn

  -- let Right sampleValue = BSBase64.decodeBase64 $ Text.encodeUtf8 "2GaCAIA="
  -- let sampleValue = value datumRes

  -- let Right plutusData = deserialiseOrFail @PlutusData.Data (BSL.fromStrict $ sampleValue)
  -- print plutusData
  -- print $ Json.encode plutusData

  forkIO $ withSocketsDo $ WS.runClient "127.0.0.1" 1337 "" (wsApp pgConn)

  let serverPort = 9999
  withStdoutLogger $ \logger -> do
    let warpSettings = W.setPort serverPort $ W.setLogger logger W.defaultSettings
    W.runSettings warpSettings (appService pgConn)
  where
    connSettings = Connection.settings "localhost" 5432 "aske" "" "ogmios-datum-cache"
