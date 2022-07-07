module Spec.Block.Parsers (spec) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.Either (isRight)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Block.Types (
  OgmiosResponse (
    OgmiosResponse,
    _methodname,
    _reflection,
    _result,
    _servicename,
    _type,
    _version
  ),
  RequestNextResult,
 )
import Data.Bifunctor (second)
import Spec.Block.Babbage qualified as Babbage

type OgmiosRequestNextResponse = OgmiosResponse RequestNextResult Int

spec :: Spec
spec = do
  describe "Ogmios JSON response parser" $ do
    it "RequestNext : Babbage rollforward" $ do
      testRequestNextResultWith Babbage.example "test/Spec/Block/Examples/RollForward_Babbage.json"

testRequestNextResultWith :: RequestNextResult -> String -> IO ()
testRequestNextResultWith response path = do
  rawFile <- ByteString.Lazy.readFile path
  let newJSON = Aeson.eitherDecode @OgmiosRequestNextResponse rawFile
  newJSON `shouldSatisfy` isRight
  second Babbage.setBabbageRawAsNull newJSON `shouldBe` (Right $ mkResponse response)

mkResponse :: RequestNextResult -> OgmiosRequestNextResponse
mkResponse result =
  OgmiosResponse
    { _type = "jsonwsp/response"
    , _version = "1.0"
    , _servicename = "ogmios"
    , _methodname = "RequestNext"
    , _result = result
    , _reflection = 0
    }
