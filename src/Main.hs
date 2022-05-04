module Main (
    main,
) where

import Control.Monad.Catch (Exception, throwM, try)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (eitherDecodeFileStrict)
import Hasql.Connection qualified as Connection
import Hasql.Connection qualified as Hasql
import Network.Wai.Handler.Warp qualified as W
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant.API.Generic (ToServantApi)
import Servant.Server (Application, Handler (..), ServerT, hoistServer, serve)
import Servant.Server.Generic (genericServerT)

import Api (Routes, datumCacheApi)
import Api.Handler (datumServiceHandlers)
import Api.Types (FirstFetchBlock (FirstFetchBlock))
import App (App (..))
import App.Env (Env (..))
import Block.Fetch (OgmiosInfo (OgmiosInfo), createStoppedFetcher)
import Block.Filter (DatumFilter (ConstFilter))
import Config (Config (..), loadConfig)
import Database (initTables)

appService :: Env -> Application
appService env = serve datumCacheApi appServer
  where
    appServer :: ServerT (ToServantApi Routes) Handler
    appServer = hoistServer datumCacheApi hoistApp appServerT

    hoistApp :: App a -> Handler a
    hoistApp = Handler . ExceptT . try . runStdoutLoggingT . flip runReaderT env . unApp

    appServerT :: ServerT (ToServantApi Routes) App
    appServerT = genericServerT datumServiceHandlers

newtype DbConnectionAcquireException = DbConnectionAcquireException Hasql.ConnectionError
    deriving stock (Eq, Show)
    deriving anyclass (Exception)

mkAppEnv :: Config -> IO Env
mkAppEnv Config{..} = do
    pgConn <- Connection.acquire cfgDbConnectionString >>= either (throwM . DbConnectionAcquireException) pure
    let firstFetchBlock = FirstFetchBlock cfgFirstFetchBlockSlot cfgFirstFetchBlockId
    ogmiosWorker <- createStoppedFetcher
    datumFilter <- case cfgDatumFilterPath of
        Nothing -> pure $ ConstFilter True
        Just path -> do
            datumFilter' <- eitherDecodeFileStrict @DatumFilter path
            case datumFilter' of
                Left e -> error e
                Right x -> pure x
    let env = Env datumFilter firstFetchBlock pgConn (OgmiosInfo cfgOgmiosPort cfgOgmiosAddress) ogmiosWorker
    print datumFilter
    pure env

main :: IO ()
main = do
    cfg@Config{..} <- loadConfig
    env <- mkAppEnv cfg
    runReaderT initTables env
    withStdoutLogger $ \logger -> do
        let warpSettings = W.setPort cfgServerPort $ W.setLogger logger W.defaultSettings
        W.runSettings warpSettings $ simpleCors (appService env)
