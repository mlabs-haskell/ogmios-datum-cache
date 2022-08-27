{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Shh.CardanoNode (writeFilem) where

import Shh qualified

-- $(Shh.loadEnv Shh.SearchPath)

rootPath = "test-env/ogmios-datum-cache-private-network"

privateNetRoot = rootPath <> "/cardano-private-testnet-setup/private-testnet"

socketPath = privateNetRoot <> "/node-bft1/node.sock"

cardanoCli = Shh.mkProc "cardano-cli"

testnetMagic = "42"

makeArg name value = "--" <> name <> "=" <> value
makeArgs = map (uncurry makeArg)
makePath x = rootPath <> x

writeFilem :: IO ()
writeFilem = Shh.runProc $
  do
    Shh.mkProc "pwd" []
    Shh.mkProc "ls" [rootPath <> "/script"]
    generateValidatorAddress
    getUserAddress

generateValidatorAddress :: Shh.Proc ()
generateValidatorAddress =
  cardanoCli $
    [ "address"
    , "build"
    ]
      <> makeArgs
        [ ("payment-script-file", makePath "/script/validator.plutus")
        , ("testnet-magic", testnetMagic)
        , ("out-file", makePath "/out/script.addr")
        ]

getUserAddress = Shh.mkProc $ "cat " <> privateNetRoot <> "/addresses/user1.addr"

queryUserUtxos user = cardanoCli $ ["query" "utxo"] <> makeArgs [("address", user), ("testnet-magic", testnetMagic)]
