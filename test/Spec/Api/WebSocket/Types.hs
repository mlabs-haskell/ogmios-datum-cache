{-# LANGUAGE QuasiQuotes #-}

module Spec.Api.WebSocket.Types (spec) where

import Data.Aeson (decode)
import Data.String.Interpolate (i)
import Test.Hspec (Spec, describe, it, shouldBe)

import Api.WebSocket.Types (
  JsonWspRequest (JsonWspRequest),
  Method (CancelFetchBlocks, GetHealthcheck, StartFetchBlocks),
 )

spec :: Spec
spec = do
  describe "Api.WebSocket.Types" $ do
    describe "decode JsonWspRequest with mirror argument" $ do
      it "mirror is a text" $ do
        decode
          [i|{"methodname": "GetHealthcheck", "mirror": "Text"}|]
          `shouldBe` Just
            (JsonWspRequest (Just "Text") GetHealthcheck)
      it "mirrot is an object " $ do
        decode
          [i|{"methodname": "GetHealthcheck", "mirror": {"field": "Text"}}|]
          `shouldBe` Just
            (JsonWspRequest (decode "{\"field\": \"Text\"}") GetHealthcheck)
    describe "parseJSON StartFetchBlocks method" $ do
      it "without token" $ do
        decode
          [i|
            {
              "methodname": "StartFetchBlocks",
              "args": {
                "slot": 1,
                "id": "ID"
              }
            }
          |]
          `shouldBe` Nothing @JsonWspRequest
      it "without token" $ do
        decode
          [i|
            {
              "methodname": "StartFetchBlocks",
              "args": {
                "slot": 1,
                "id": "ID",
                "token": "X"
              }
            }
          |]
          `shouldBe` Just
            (JsonWspRequest Nothing $ StartFetchBlocks 1 "ID" Nothing "X")
    describe "parseJSON CancelFetchBlocks method" $ do
      it "with token" $ do
        decode
          [i|{"methodname": "CancelFetchBlocks", "args": {"token": "X"}}|]
          `shouldBe` Just
            (JsonWspRequest Nothing $ CancelFetchBlocks "X")
      it "without token in args" $ do
        decode [i|{"methodname": "CancelFetchBlocks", "args": {}}|]
          `shouldBe` Nothing @JsonWspRequest

      it "without args" $ do
        decode [i|{"methodname": "CancelFetchBlocks"}|]
          `shouldBe` Nothing @JsonWspRequest
