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
  RequestNextResult (RollForward),
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
        Babbage.example
        "test/Spec/Block/Examples/RollForward_Babbage.json"
    it "RequestNext : Alonzo rollforward" $ do
      testRequestNextResultWith
        Alonzo.example
        "test/Spec/Block/Examples/RollForward_Alonzo.json"

testRequestNextResultWith :: RequestNextResult -> String -> IO ()
testRequestNextResultWith response path = do
  rawFile <- ByteString.Lazy.readFile path
  let newJSON = Aeson.eitherDecode @OgmiosRequestNextResponse rawFile
  newJSON `shouldSatisfy` isRight
  second cutResponse newJSON `shouldBe` (Right $ mkResponse response)

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
