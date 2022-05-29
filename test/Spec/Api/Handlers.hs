{-# LANGUAGE QuasiQuotes #-}

module Spec.Api.Handlers (spec) where

import Data.ByteString (ByteString)
import Data.ByteString.Base64 (encode)
import Network.HTTP.Types.Method (methodPost)
import Network.Wai.Test (SResponse)
import Servant.Server (Application)
import Test.Hspec (Spec, describe, it, pendingWith)
import Test.Hspec.Wai (
  ResponseMatcher (matchStatus),
  WaiSession,
  get,
  post,
  request,
  shouldRespondWith,
  with,
 )
import Test.Hspec.Wai.JSON (json)

spec :: Either String (Application, Application) -> Spec
spec (Left err) =
  it "Handlers" $ pendingWith $ "test-env not runned: " <> err
spec (Right (app, appWithAuth)) =
  describe "Handlers" $ do
    withoutAuthSpec app
    withAuthSpec appWithAuth

withoutAuthSpec :: Application -> Spec
withoutAuthSpec app =
  describe "Protect Privileged API is not configured" $
    with (return app) $ do
      it "/healthcheck - 200" $ get "/healthcheck" `shouldRespondWith` 200
      it "/control/cancel_fetch_blocks - 422 Unprocessable Entity" $ do
        post "/control/cancel_fetch_blocks" ""
          `shouldRespondWith` [json|{"error": "No block fetcher running"}|]
            { matchStatus = 422
            }
      it "/control/cancel_fetch_blocks with auth - 422 Unprocessable Entity" $ do
        postWithAuth "test:test" "/control/cancel_fetch_blocks"
          `shouldRespondWith` [json|{"error": "No block fetcher running"}|]
            { matchStatus = 422
            }
      it "/restricted_control/cancel_fetch_blocks - 401 Unauthorized" $ do
        post "/restricted_control/cancel_fetch_blocks" ""
          `shouldRespondWith` 401
      it "/restricted_control/cancel_fetch_blocks with any auth - 403 Forbidden" $ do
        postWithAuth "any:any" "/restricted_control/cancel_fetch_blocks"
          `shouldRespondWith` 403

withAuthSpec :: Application -> Spec
withAuthSpec appWithAuth =
  describe "Protect Privileged API is configured" $
    with (return appWithAuth) $ do
      it "/healthcheck - 200" $ get "/healthcheck" `shouldRespondWith` 200
      it "/control/cancel_fetch_blocks - 303 See Other" $ do
        post "/control/cancel_fetch_blocks" "" `shouldRespondWith` 303
      it "/restricted_control/cancel_fetch_blocks - 401 Unauthorized" $ do
        post "/restricted_control/cancel_fetch_blocks" ""
          `shouldRespondWith` 401
      it "/restricted_control/cancel_fetch_blocks with auth - 422 Unprocessable Entity" $ do
        postWithAuth "test:test" "/restricted_control/cancel_fetch_blocks"
          `shouldRespondWith` [json|{"error": "No block fetcher running"}|]
            { matchStatus = 422
            }
      it "/restricted_control/cancel_fetch_blocks with wrong auth - 403 Forbidden" $ do
        postWithAuth "wrong:wrong" "/restricted_control/cancel_fetch_blocks"
          `shouldRespondWith` 403

postWithAuth :: ByteString -> ByteString -> WaiSession st SResponse
postWithAuth auth path = request methodPost path headers ""
  where
    headers = [("Authorization", "Basic " <> encode auth)]
