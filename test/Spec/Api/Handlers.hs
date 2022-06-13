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

spec :: Either String Application -> Spec
spec (Left err) =
  it "Handlers" $ pendingWith $ "test-env not runned: " <> err
spec (Right app) = do
  describe "Handlers" $ do
    describe "Protect Privileged API is configured" $
      with (return app) $ do
        it "/healthcheck - 200" $ get "/healthcheck" `shouldRespondWith` 200
        it "/control/cancel_fetch_blocks - 401 Unauthorized" $ do
          post "/control/cancel_fetch_blocks" ""
            `shouldRespondWith` 401
        it "/control/cancel_fetch_blocks with wrong auth - 403 Forbidden" $ do
          postWithAuth "wrong:wrong" "/control/cancel_fetch_blocks"
            `shouldRespondWith` 403
        it "/control/cancel_fetch_blocks with auth - 422 Unprocessable Entity" $ do
          postWithAuth "usr:pwd" "/control/cancel_fetch_blocks"
            `shouldRespondWith` [json|{"error": "No block fetcher running"}|]
              { matchStatus = 422
              }

postWithAuth :: ByteString -> ByteString -> WaiSession st SResponse
postWithAuth auth path = request methodPost path headers ""
  where
    headers = [("Authorization", "Basic " <> encode auth)]
