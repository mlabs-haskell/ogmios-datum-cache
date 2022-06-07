module Spec.Api.Handlers (spec) where

import Servant.Server (Application)
import Test.Hspec (Spec, describe, it, pendingWith)
import Test.Hspec.Wai (get, shouldRespondWith, with)

spec :: Either String Application -> Spec
spec (Left err) =
  it "Handlers" $ pendingWith $ "test-env not runned: " <> err
spec (Right app) =
  describe "Handlers" $ do
    withoutAuthSpec app

withoutAuthSpec :: Application -> Spec
withoutAuthSpec app =
  describe "Protect Privileged API is not configured" $
    with (return app) $ do
      it "/healthcheck - 200" $ get "/healthcheck" `shouldRespondWith` 200
