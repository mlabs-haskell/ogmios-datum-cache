module Spec.Api.WebSocket.Types (spec) where

import Api.WebSocket.Types (
  JsonWspRequest (JsonWspRequest),
  Method (GetBlock),
 )
import Data.Aeson (decode)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Api.WebSocket.Types" $ do
    describe "decode JsonWspRequest with mirror argument" $ do
      it "mirror is a text" $ do
        decode "{\"methodname\":\"GetBlock\", \"mirror\": \"Text\"}"
          `shouldBe` Just
            (JsonWspRequest (Just "Text") GetBlock)
      it "mirrot is an object " $ do
        decode "{\"methodname\":\"GetBlock\", \"mirror\": {\"field\": \"Text\"}}"
          `shouldBe` Just
            (JsonWspRequest (decode "{\"field\": \"Text\"}") GetBlock)
