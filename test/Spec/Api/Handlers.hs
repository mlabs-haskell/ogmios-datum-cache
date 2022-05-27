{-# LANGUAGE QuasiQuotes #-}

module Spec.Api.Handlers (spec) where

import Data.ByteString (ByteString)
import Network.HTTP.Types.Method (methodPost)
import Network.Wai.Test (SResponse)
import Servant.Server (Application)
import Test.Hspec (Spec, describe, expectationFailure, it, xit)
import Test.Hspec.Wai (
  WaiSession,
  get,
  matchStatus,
  post,
  request,
  shouldRespondWith,
  with,
 )
import Test.Hspec.Wai.JSON (json)

spec :: Either String Application -> Spec
spec (Left err) =
  it "Handlers" $
    expectationFailure $ "Test environment broken: " <> err
spec (Right app) = with (return app) $
  describe "Handlers" $ do
    it "healthcheck" $ get "/healthcheck" `shouldRespondWith` 200
    describe "Protect Privileged API not configured" $ do
      xit "cancel_fetch_blocks" $ do
        post "/control/cancel_fetch_blocks" ""
          `shouldRespondWith` [json|{"error": "No block fetcher running"}|]
            { matchStatus = 422
            }
      it "cancel_fetch_blocks" $ do
        postWithAuth "/control/cancel_fetch_blocks"
          `shouldRespondWith` [json|{"error": "No block fetcher running"}|]
            { matchStatus = 422
            }

postWithAuth :: ByteString -> WaiSession st SResponse
postWithAuth path = request methodPost path [auth] ""
  where
    auth = ("Authorization", "Basic dGVzdDp0ZXN0") -- test:test
