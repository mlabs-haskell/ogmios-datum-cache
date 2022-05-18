module Main (main) where

import Test.Hspec (hspec)

import Spec.Api.WebSocket.Types qualified
import Spec.Block.Fetch qualified
import Spec.Config qualified

main :: IO ()
main = hspec $ do
  Spec.Api.WebSocket.Types.spec
  Spec.Block.Fetch.spec
  Spec.Config.spec
