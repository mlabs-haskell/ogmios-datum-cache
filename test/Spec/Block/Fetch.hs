module Spec.Block.Fetch (spec) where

import Control.Monad.Reader.Has (Has, runReader)
import GHC.Generics (Generic)
import Test.Hspec (Spec, describe, it, shouldBe)

import Block.Fetch (ControlApiToken (..), withControlApiTokenToken)

spec :: Spec
spec = describe "Block.Fetch" $ do
  describe "withControlApiTokenToken" $ do
    describe "server.controlApiToken not setted" $ do
      it "ApiTokenToken not granted" $
        withControlApiTokenToken' (env Nothing) Nothing `shouldBe` Right ()
      it "ApiTokenToken granted" $
        withControlApiTokenToken' (env Nothing) (Just "x") `shouldBe` Right ()
    describe "server.controlApiToken setted" $ do
      it "ApiTokenToken not granted" $
        withControlApiTokenToken' (env $ Just "X") Nothing `shouldBe` Left ()
      it "ApiTokenToken granted, but wrong" $
        withControlApiTokenToken' (env $ Just "X") (Just "Y") `shouldBe` Left ()
      it "ApiTokenToken granted" $
        withControlApiTokenToken' (env $ Just "X") (Just "X") `shouldBe` Right ()

withControlApiTokenToken' :: Env -> Maybe String -> Either () ()
withControlApiTokenToken' env_ token =
  flip runReader env_ $
    withControlApiTokenToken () token (return $ Right ())

newtype Env = Env
  { envControlApiToken :: ControlApiToken
  }
  deriving stock (Generic)
  deriving anyclass (Has ControlApiToken)

env :: Maybe String -> Env
env = Env . ControlApiToken
