{-# LANGUAGE ScopedTypeVariables #-}

module Spec.Block.Parsers (spec) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.Either (isRight)
import Data.Kind (Type)
import Data.Text qualified as Text
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Block.Types (
  CursorPoint (CursorOrigin, CursorPoint),
  FindIntersectResult (IntersectionFound, IntersectionNotFound),
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
import Spec.Block.Byron qualified as Byron

type OgmiosRequestNextResponse = OgmiosResponse RequestNextResult Int

spec :: Spec
spec = do
  describe "Ogmios JSON response parser" $ do
    it "RequestNext : Babbage rollforward" $ do
      testRequestNextResultWith
        cutResponse
        (mkNextResponse Babbage.example)
        "test/Spec/Block/Examples/RollForward_Babbage.json"
    it "RequestNext : Alonzo rollforward" $ do
      testRequestNextResultWith
        cutResponse
        (mkNextResponse Alonzo.example)
        "test/Spec/Block/Examples/RollForward_Alonzo.json"
    it "RequestNext : Byron rollforward" $ do
      testRequestNextResultWith
        cutResponse
        (mkNextResponse Byron.example)
        "test/Spec/Block/Examples/RollForward_Byron.json"
    it "RequestNext : RollBackward non origin" $ do
      testRequestNextResultWith
        id
        (mkNextResponse forwardResultNonOrigin)
        "test/Spec/Block/Examples/RollBackward_NonOrigin.json"
    it "RequestNext : RollBackward origin" $ do
      testRequestNextResultWith
        id
        (mkNextResponse forwardResultOrigin)
        "test/Spec/Block/Examples/RollBackward_Origin.json"
    it "FindIntersect : Intersection found" $ do
      testRequestNextResultWith
        id
        (mkResponse "FindIntersect" intersectionFound)
        "test/Spec/Block/Examples/IntersectionFound.json"
    it "FindIntersect : Intersection not found" $ do
      testRequestNextResultWith
        id
        (mkResponse "FindIntersect" intersectionNotFound)
        "test/Spec/Block/Examples/IntersectionNotFound.json"
  where
    mkNextResponse = mkResponse "RequestNext"

testRequestNextResultWith ::
  forall (a :: Type).
  (Show a, Eq a, Aeson.FromJSON a) =>
  (OgmiosResponse a Int -> OgmiosResponse a Int) ->
  OgmiosResponse a Int ->
  String ->
  IO ()
testRequestNextResultWith modify testResponse path = do
  rawFile <- ByteString.Lazy.readFile path
  let newJSON :: Either String (OgmiosResponse a Int)
      newJSON = Aeson.eitherDecode rawFile
  newJSON `shouldSatisfy` isRight
  second modify newJSON `shouldBe` Right testResponse

mkResponse :: forall (a :: Type). Text.Text -> a -> OgmiosResponse a Int
mkResponse name result =
  OgmiosResponse
    { _type = "jsonwsp/response"
    , _version = "1.0"
    , _servicename = "ogmios"
    , _methodname = name
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

intersectionFound :: FindIntersectResult
intersectionFound =
  IntersectionFound
    ( CursorPoint
        61625527
        "3afd8895c7b270f8250b744ec8d2b3c53ee2859c9d5711d906c47fe51b800988"
    )
    $ ResultTip
      { slot = 62284316
      , hash = "30b253b54dbaf6fbacef0f3cb5c46dde178044406a7022672d8ee9c94034649a"
      , blockNo = 3673647
      }
intersectionNotFound :: FindIntersectResult
intersectionNotFound =
  IntersectionNotFound $
    ResultTip
      { slot = 63040785
      , hash = "2d972be6ff4f3d1a8442e122dafd4baa5073127c6883fe17562d7fb07d453efc"
      , blockNo = 3696087
      }

{- | This function allow us to cut the response size, keeping just
 the three initial TX.
-}
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
            (AlonzoBlock (Types.Alonzo.Block {..})) ->
              let newTransactions =
                    take 3 $ rawTransactions
                  newBody = take 3 body
               in AlonzoBlock $
                    Types.Alonzo.Block
                      newBody
                      newTransactions
                      header
                      headerHash
            _ -> someBlock
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
