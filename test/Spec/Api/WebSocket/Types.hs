module Spec.Api.WebSocket.Types (spec) where

import Api.WebSocket.Types (
  JsonWspRequest (JsonWspRequest),
  Method (GetHealthcheck),
 )
import Data.Aeson (decode)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Api.WebSocket.Types" $ do
    describe "decode JsonWspRequest with mirror argument" $ do
      it "mirror is a text" $ do
        decode "{\"methodname\":\"GetHealthcheck\", \"mirror\": \"Text\"}"
          `shouldBe` Just
            (JsonWspRequest (Just "Text") GetHealthcheck)
      it "mirrot is an object " $ do
        decode "{\"methodname\":\"GetHealthcheck\", \"mirror\": {\"field\": \"Text\"}}"
          `shouldBe` Just
            (JsonWspRequest (decode "{\"field\": \"Text\"}") GetHealthcheck)
