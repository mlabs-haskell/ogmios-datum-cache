module Spec.Block.Parsers (spec) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.Either (isRight)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Block.Types (
  CursorPoint (CursorOrigin, CursorPoint),
  OgmiosResponse (
    OgmiosResponse,
    _methodname,
    _reflection,
    _result,
    _servicename,
    _type,
    _version
  ),
  RequestNextResult (RollBackward, RollForward),
  ResultTip (ResultTip, blockNo, hash, slot),
  SomeBlock (AlonzoBlock, BabbageBlock),
 )
import Block.Types.Alonzo qualified as Types.Alonzo
import Block.Types.Babbage qualified as Types.Babbage
import Data.Bifunctor (second)
import Spec.Block.Alonzo qualified as Alonzo
import Spec.Block.Babbage qualified as Babbage

type OgmiosRequestNextResponse = OgmiosResponse RequestNextResult Int

spec :: Spec
spec = do
  describe "Ogmios JSON response parser" $ do
    it "RequestNext : Babbage rollforward" $ do
      testRequestNextResultWith
        cutResponse
        Babbage.example
        "test/Spec/Block/Examples/RollForward_Babbage.json"
    it "RequestNext : Alonzo rollforward" $ do
      testRequestNextResultWith
        cutResponse
        Alonzo.example
        "test/Spec/Block/Examples/RollForward_Alonzo.json"
    it "RequestNext : RollBackward non origin" $ do
      testRequestNextResultWith
        id
        forwardResultNonOrigin
        "test/Spec/Block/Examples/RollBackward_NonOrigin.json"
    it "RequestNext : RollBackward origin" $ do
      testRequestNextResultWith
        id
        forwardResultOrigin
        "test/Spec/Block/Examples/RollBackward_Origin.json"

testRequestNextResultWith ::
  (OgmiosRequestNextResponse -> OgmiosRequestNextResponse) ->
  RequestNextResult ->
  String ->
  IO ()
testRequestNextResultWith modify response path = do
  rawFile <- ByteString.Lazy.readFile path
  let newJSON = Aeson.eitherDecode @OgmiosRequestNextResponse rawFile
  newJSON `shouldSatisfy` isRight
  second modify newJSON `shouldBe` Right (mkResponse response)

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

forwardResultNonOrigin :: RequestNextResult
forwardResultNonOrigin =
  RollBackward
    ( CursorPoint
        61625527
        "3afd8895c7b270f8250b744ec8d2b3c53ee2859c9d5711d906c47fe51b800988"
    )
    $ ResultTip
      { slot = 62284316
      , hash =
          "30b253b54dbaf6fbacef0f3cb5c46dde178044406a7022672d8ee9c94034649a"
      , blockNo = 3673647
      }

forwardResultOrigin :: RequestNextResult
forwardResultOrigin =
  RollBackward
    CursorOrigin
    $ ResultTip
      { slot = 62276801
      , hash =
          "ceaa4558d36026b575c2a5860e73ee9bd5fccc535e71386b03380b48fa3b52f3"
      , blockNo = 3673441
      }

cutResponse :: OgmiosRequestNextResponse -> OgmiosRequestNextResponse
cutResponse
  ( OgmiosResponse
      ty
      ver
      ser
      met
      ( RollForward
          someBlock
          resTip
        )
      ref
    ) =
    let newBlock =
          case someBlock of
            (BabbageBlock (Types.Babbage.Block {..})) ->
              let newTransactions =
                    map (\raw -> raw{rawTx = Aeson.Null}) rawTransactions
               in BabbageBlock $
                    Types.Babbage.Block
                      body
                      newTransactions
                      header
                      headerHash
            (AlonzoBlock (Types.Alonzo.Block {..})) ->
              let newTransactions =
                    take 3 $
                      map
                        (\raw -> raw{rawTx = Aeson.Null})
                        rawTransactions
                  newBody = take 3 body
               in AlonzoBlock $
                    Types.Alonzo.Block
                      newBody
                      newTransactions
                      header
                      headerHash
            _ -> newBlock
     in OgmiosResponse
          ty
          ver
          ser
          met
          ( RollForward
              newBlock
              resTip
          )
          ref
cutResponse a = a
