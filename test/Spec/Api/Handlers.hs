{-# LANGUAGE QuasiQuotes #-}

module Spec.Api.Handlers (spec) where

import Data.ByteString (ByteString)
import Data.ByteString.Base64 (encode)
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

spec :: Either String (Application, Application) -> Spec
spec (Left err) =
  it "Handlers" $
    expectationFailure $ "Test environment broken: " <> err
spec (Right (app, appWithAuth)) =
  describe "Handlers" $ do
    withoutAuthSpec app
    withAuthSpec appWithAuth

withoutAuthSpec :: Application -> Spec
withoutAuthSpec app =
  describe "Protect Privileged API is not configured" $
    with (return app) $ do
      it "healthcheck" $ get "/healthcheck" `shouldRespondWith` 200
      -- FIXME: require endpoint without auth
      xit "cancel_fetch_blocks" $ do
        post "/control/cancel_fetch_blocks" ""
          `shouldRespondWith` [json|{"error": "No block fetcher running"}|]
            { matchStatus = 422
            }
      it "cancel_fetch_blocks" $ do
        postWithAuth "test:test" "/control/cancel_fetch_blocks"
          `shouldRespondWith` [json|{"error": "No block fetcher running"}|]
            { matchStatus = 422
            }

withAuthSpec :: Application -> Spec
withAuthSpec appWithAuth =
  describe "Protect Privileged API is configured" $
    with (return appWithAuth) $ do
      it "healthcheck" $ get "/healthcheck" `shouldRespondWith` 200
      it "cancel_fetch_blocks without auth" $ do
        post "/control/cancel_fetch_blocks" ""
          `shouldRespondWith` 401
      it "cancel_fetch_blocks with wrong auth" $ do
        postWithAuth "wrong:wrong" "/control/cancel_fetch_blocks"
          `shouldRespondWith` 403
      it "cancel_fetch_blocks with auth" $ do
        postWithAuth "test:test" "/control/cancel_fetch_blocks"
          `shouldRespondWith` [json|{"error": "No block fetcher running"}|]
            { matchStatus = 422
            }

postWithAuth :: ByteString -> ByteString -> WaiSession st SResponse
postWithAuth auth path = request methodPost path headers ""
  where
    headers = [("Authorization", "Basic " <> encode auth)]
