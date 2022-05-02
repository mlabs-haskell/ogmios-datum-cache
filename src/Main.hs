module Main (
    main,
) where

import Colog qualified
import Control.Concurrent.MVar (newEmptyMVar, newMVar)
import Control.Monad.Catch (Exception, throwM, try)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.Reader (runReaderT)
import Data.Aeson (eitherDecodeFileStrict)
import Data.Set qualified as Set
import Hasql.Connection qualified as Connection
import Hasql.Connection qualified as Hasql
import Hasql.Session qualified as Session
import Network.Wai.Handler.Warp qualified as W
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant.API.Generic (ToServantApi)
import Servant.Server (Application, Handler (..), ServerT, hoistServer, serve)
import Servant.Server.Generic (genericServerT)

import Api (Routes, datumCacheApi)
import Api.Handler (datumServiceHandlers)
import App (App (..))
import App.Env (Env (..))
import App.FirstFetchBlock (FirstFetchBlock (..))
import Block.Filter (DatumFilter (ConstFilter))
import Config (Config (..), loadConfig)
import Database (initTables)

appService :: Env App -> Application
appService env = serve datumCacheApi appServer
  where
    appServer :: ServerT (ToServantApi Routes) Handler
    appServer = hoistServer datumCacheApi hoistApp appServerT

    hoistApp :: App a -> Handler a
    hoistApp = Handler . ExceptT . try . flip runReaderT env . unApp

    appServerT :: ServerT (ToServantApi Routes) App
    appServerT = genericServerT datumServiceHandlers

newtype DbConnectionAcquireException = DbConnectionAcquireException Hasql.ConnectionError
    deriving stock (Eq, Show)
    deriving anyclass (Exception)

mkAppEnv :: Config -> IO (Env App)
mkAppEnv Config{..} = do
    pgConn <- Connection.acquire cfgDbConnectionString >>= either (throwM . DbConnectionAcquireException) pure
    requestedDatumHashes <- newMVar Set.empty
    let firstFetchBlock = FirstFetchBlock cfgFirstFetchBlockSlot cfgFirstFetchBlockId
    ogmiosWorker <- newEmptyMVar
    datumFilter <- case cfgDatumFilterPath of
        Nothing -> pure $ ConstFilter True
        Just path -> do
            datumFilter' <- eitherDecodeFileStrict @DatumFilter path
            case datumFilter' of
                Left e -> error e
                Right x -> pure x
    let env = Env requestedDatumHashes datumFilter firstFetchBlock pgConn Colog.richMessageAction cfgOgmiosAddress cfgOgmiosPort ogmiosWorker
    print datumFilter
    pure env

main :: IO ()
main = do
    cfg@Config{..} <- loadConfig
    env <- mkAppEnv cfg
    _ <- Session.run initTables $ envDbConnection env
    withStdoutLogger $ \logger -> do
        let warpSettings = W.setPort cfgServerPort $ W.setLogger logger W.defaultSettings
        W.runSettings warpSettings $ simpleCors (appService env)
