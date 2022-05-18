module Main (main) where

import Spec.Api.WebSocket.Types qualified
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  Spec.Api.WebSocket.Types.spec
