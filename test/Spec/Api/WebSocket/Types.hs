{-# LANGUAGE QuasiQuotes #-}

module Spec.Api.WebSocket.Types (spec) where

import Data.Aeson (decode)
import Data.String.Interpolate (i)
import Test.Hspec (Spec, describe, it, shouldBe)

import Api.WebSocket.Types (
  JsonWspRequest (JsonWspRequest),
  Method (GetHealthcheck, SetStartingBlock),
 )
import Block.Types (BlockInfo (BlockInfo))

spec :: Spec
spec = do
  describe "Api.WebSocket.Types" $ do
    describe "decode JsonWspRequest with mirror argument" $ do
      it "mirror is a text" $ do
        decode
          [i|{"methodname": "GetHealthcheck", "mirror": "Text"}|]
          `shouldBe` Just
            (JsonWspRequest (Just "Text") GetHealthcheck)
      it "mirror is an object " $ do
        decode
          [i|{"methodname": "GetHealthcheck", "mirror": {"field": "Text"}}|]
          `shouldBe` Just
            (JsonWspRequest (decode "{\"field\": \"Text\"}") GetHealthcheck)
    describe "parseJSON StartFetchBlocks method" $ do
      it "without token" $ do
        decode
          [i|
            {
              "methodname": "SetStartingBlock",
              "args": {
                "startingBlock": {
                  "blockSlot": 1,
                  "blockId": "ID"
                }
              }
            }
          |]
          `shouldBe` Nothing @JsonWspRequest
      it "with token" $ do
        decode
          [i|
            {
              "methodname": "SetStartingBlock",
              "args": {
                "startingBlock": {
                  "blockSlot": 1,
                  "blockId": "ID"
                },
                "token": "X"
              }
            }
          |]
          `shouldBe` Just
            (JsonWspRequest Nothing $ SetStartingBlock "X" (BlockInfo 1 "ID"))
