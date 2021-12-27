{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main
    ( main
    ) where

import Control.Exception (throwIO, Exception)
import           Control.Concurrent  (forkIO, threadDelay)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS
import qualified Data.Aeson as Json
import Data.Aeson (ToJSON, FromJSON, withObject, (.:))
import Data.Map (Map)
import GHC.Generics (Generic)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM

type OgmiosMirror = Int

data CursorPoint = CursorPoint
  { slot :: Integer
  , hash :: Text
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data CursorPoints = CursorPoints
  { points :: [CursorPoint]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass ToJSON

data OgmiosRequest args mirror = OgmiosRequest
  { _type :: Text
  , _version :: Text
  , _servicename :: Text
  , _methodname :: Text
  , _args :: args
  , _mirror :: mirror
  } deriving stock (Eq, Show, Generic)

instance (ToJSON args, ToJSON mirror) => ToJSON (OgmiosRequest args mirror) where
  toJSON = Json.genericToJSON Json.defaultOptions { Json.fieldLabelModifier = drop 1 }

type OgmiosFindIntersectRequest = OgmiosRequest CursorPoints OgmiosMirror

type OgmiosRequestNextRequest = OgmiosRequest (Map Text Text) OgmiosMirror

sampleFindIntersectRequest :: OgmiosFindIntersectRequest
sampleFindIntersectRequest = OgmiosRequest
  { _type = "jsonwsp/request"
  , _version = "1.0"
  , _servicename = "ogmios"
  , _methodname = "FindIntersect"
  , _args = points
  , _mirror = 15 --Map.fromList [("n", 15)]
  }
  where
    points =
      CursorPoints [CursorPoint 44366242 "d2a4249fe3d0607535daa26caf12a38da2233586bc51e79ed0b3a36170471bf5"]

mkRequestNextRequest :: Int -> OgmiosRequestNextRequest
mkRequestNextRequest n = OgmiosRequest
  { _type = "jsonwsp/request"
  , _version = "1.0"
  , _servicename = "ogmios"
  , _methodname = "RequestNext"
  , _args = Map.empty
  , _mirror = n
  }

findIntersect1 :: BSL.ByteString
findIntersect1 = Json.encode sampleFindIntersectRequest

data OgmiosResponse result reflection = OgmiosResponse
  { _type :: Text
  , _version :: Text
  , _servicename :: Text
  , _methodname :: Text
  , _result :: result
  , _reflection :: reflection
  } deriving stock (Eq, Show, Generic)

instance (FromJSON result, FromJSON reflection) => FromJSON (OgmiosResponse result reflection) where
  parseJSON = Json.genericParseJSON Json.defaultOptions { Json.fieldLabelModifier = drop 1 }

type OgmiosFindIntersectResponse = OgmiosResponse FindIntersectResult OgmiosMirror

data ResultTip = ResultTip
  { slot :: Integer
  , hash :: Text
  , blockNo :: Integer
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON)

data FindIntersectResult =
  IntersectionFound { point :: CursorPoint
                    , tip :: ResultTip
                    }
  | IntersectionNotFound { tip :: ResultTip}
  deriving stock (Eq, Show, Generic)

instance FromJSON FindIntersectResult where
  parseJSON = withObject "FindIntersectResult" $ \o -> do
    case HM.toList o of
      [("IntersectionFound", intersectionObj)] ->
        withObject "IntersectionFound" (\obj -> do
          point <- obj .: "point"
          tip <- obj .: "tip"
          pure $ IntersectionFound point tip) intersectionObj

      [("IntersectionNotFound", intersectionObj)] ->
        withObject "IntersectionNotFound" (\obj -> do
          tip <- obj .: "tip"
          pure $ IntersectionNotFound tip) intersectionObj

      _ -> fail "Unexpected object key"

type OgmiosRequestNextResponse = OgmiosResponse RequestNextResult OgmiosMirror

data RequestNextResult =
  RollBackward { point :: CursorPoint
               , tip :: ResultTip
               }
 | RollForward { block :: Block
               , tip :: ResultTip }

 deriving stock (Eq, Show, Generic)

data Block =
  OtherBlock
  | MkAlonzoBlock AlonzoBlock
  deriving stock (Eq, Show, Generic)

data AlonzoBlockHeader = AlonzoBlockHeader
  { signature :: Text
  , nonce :: AlonzoBlockHeaderNonce
  , leadervalue ::  AlonzoBlockHeaderLeaderValue
  }


data AlonzoBlock = AlonzoBlock
  { body :: [Transaction]
  , header :: AlonzoBlockHeader
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass FromJSON

instance FromJSON RequestNextResult where
  parseJSON = withObject "RequestNextResult" $ \o -> do
    case HM.toList o of
      [("RollBackward", rollObj)] ->
        withObject "RollBackward" (\obj -> do
          point <- obj .: "point"
          tip <- obj .: "tip"
          pure $ RollBackward point tip) rollObj

      [("RollForward", rollObj)] ->
        withObject "RollForward" (\obj -> do
          tip <- obj .: "tip"
          blockObj <- obj .: "block"
          case HM.toList blockObj of
            [("alonzo" :: Text, blockValue)] -> do
              block <- Json.parseJSON @AlonzoBlock blockValue
              pure $ RollForward (MkAlonzoBlock block) tip
            [(_, blockObj)] ->
              pure $ RollForward OtherBlock tip) rollObj

      _ -> fail "Unexpected object key"


data FindIntersectException = FindIntersectException Text
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

receiveLoop :: WS.Connection -> IO ()
receiveLoop conn = do
  jsonMsg <- WS.receiveData conn
  print jsonMsg

  -- TODO: throwM
  msg <- maybe (throwIO $ FindIntersectException "Can't decode response") pure $ Json.decode @OgmiosFindIntersectResponse jsonMsg
  print msg
  case _result msg of
    IntersectionNotFound _ -> do
      error "Intersection not found"
    IntersectionFound _ _ -> do
      forkIO $ receiveBlocksLoop conn
      requestRemainingBlocks conn

requestRemainingBlocks :: WS.Connection -> IO ()
requestRemainingBlocks conn = forever $ do
  WS.sendTextData conn (Json.encode $ mkRequestNextRequest 0)
  threadDelay 1000000

receiveBlocksLoop :: WS.Connection -> IO ()
receiveBlocksLoop conn = forever $ do
  jsonMsg <- WS.receiveData conn
  T.putStrLn jsonMsg
  threadDelay 1000000

  -- msg <- Json.decode @OgmiosRequestNextResponse

app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"
    forkIO $ receiveLoop conn

    WS.sendTextData conn findIntersect1
    threadDelay 10000000
    WS.sendClose conn ("Bye!" :: Text)

main :: IO ()
main = withSocketsDo $ WS.runClient "127.0.0.1" 1337 "" app
