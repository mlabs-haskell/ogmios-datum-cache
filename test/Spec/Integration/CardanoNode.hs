{-# LANGUAGE TemplateHaskell #-}

module Spec.Integration.CardanoNode (loockAndRelease) where

import Control.Monad (when)
import Control.Monad.Catch (Exception, throwM)
import Data.Bifunctor (first)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (mapMaybe)
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Encoding qualified as Encoding
import System.Process.Typed (ProcessConfig)
import System.Process.Typed qualified as Typed

cardanoEnv :: ProcessConfig stdin stdout stderr -> ProcessConfig stdin stdout stderr
cardanoEnv =
  Typed.setEnv [("CARDANO_NODE_SOCKET_PATH", "cardano-private-testnet-setup/private-testnet/node-bft1/node.sock")]
    . Typed.setWorkingDir "test-env/ogmios-datum-cache-private-network"

privateNetRoot = "cardano-private-testnet-setup/private-testnet"

socketPath = privateNetRoot <> "/node-bft1/node.sock"

cardanoCli = Typed.proc "cardano-cli"

testnetMagic = "42"

type Address = String
type TxHash = String

makeArg name value = "--" <> name <> lastValue
  where
    lastValue = if null value then "" else "=" <> value
makeArgs = map (uncurry makeArg)
makePath x = "./" <> x

makeHash :: String -> Int -> String
makeHash hash iD = hash <> "#" <> show iD

data CardanoNodeError
  = Can'tGetUserAddress
  | Can'tGetScriptAddress
  | Can'tGetUtxosAt Address
  | NotFounds
  deriving (Show)

instance Exception CardanoNodeError

loockAndRelease :: IO ()
loockAndRelease =
  do
    run $ generateValidatorAddress
    (userAddress, _) <- readP_ getUserAddress
    when (null userAddress) (throwM Can'tGetUserAddress)
    putStrLn userAddress
    (scriptAddress, _) <- readP_ getScriptAddress
    putStrLn scriptAddress
    (originalTxHash, originalTxId, originalAmount) <- getFirstUTxO userAddress
    when (originalAmount == 0) (throwM NotFounds)
    let txAmount = div originalAmount 2
    run $
      buildLockTransaction
        userAddress
        (makeHash originalTxHash originalTxId)
        scriptAddress
        txAmount
        "script/datum.json"
    run signTransaction
    run submitTransaction
    run $ Typed.proc "sleep" ["2"]
    (scriptTxHash, scriptTxId, _) <- getFirstUTxO scriptAddress
    (newUserTxHash, newUserTxId, _) <- getFirstUTxO userAddress
    run $
      buildConsumeTransaction
        userAddress
        (makeHash scriptTxHash scriptTxId)
        "script/datum.json"
        (makeHash newUserTxHash newUserTxId)
    run signTransaction
    run submitTransaction
    run $ Typed.proc "sleep" ["2"]
    (lastUTxOs, _) <- (readP_ . queryUtxos) userAddress
    putStrLn lastUTxOs
  where
    run :: ProcessConfig stdin stdout stderr -> IO ()
    run = Typed.runProcess_ . cardanoEnv

toString :: ByteString -> String
toString = Text.Lazy.unpack . Encoding.decodeUtf8

readP_ :: ProcessConfig stdin stdout stderr -> IO (String, ByteString)
readP_ command =
  first toString
    <$> (Typed.readProcess_ . cardanoEnv) command

parseUtxoQuery :: String -> [(String, Int, Int)]
parseUtxoQuery input =
  case lines input of
    header : x : remain -> mapMaybe parse (x : remain)
    _ -> []
  where
    parse :: String -> Maybe (String, Int, Int)
    parse str =
      case words str of
        txHash : txId : amount : _ -> pure (txHash, (read txId), (read amount))
        _ -> Nothing

generateValidatorAddress :: ProcessConfig () () ()
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

getUserAddress :: ProcessConfig () () ()
getUserAddress = Typed.proc "cat" [privateNetRoot <> "/addresses/user1.addr"]

queryUtxos :: Address -> ProcessConfig () () ()
queryUtxos address =
  cardanoCli $
    ["query", "utxo"]
      <> makeArgs [("address", address), ("testnet-magic", testnetMagic)]

getFirstUTxO :: Address -> IO (String, Int, Int)
getFirstUTxO address =
  do
    (rawUTxOs, _) <- (readP_ . queryUtxos) address
    putStrLn rawUTxOs
    case parseUtxoQuery rawUTxOs of
      [] -> throwM $ Can'tGetUtxosAt address
      x : _ -> pure x

getScriptAddress :: ProcessConfig () () ()
getScriptAddress = Typed.proc "cat" ["out/script.addr"]

buildLockTransaction ::
  Address ->
  TxHash ->
  Address ->
  Int ->
  String ->
  ProcessConfig () () ()
buildLockTransaction userAddress txHash scriptAddress amount datumPath =
  cardanoCli $
    ["transaction", "build"]
      <> makeArgs
        [ ("alonzo-era", "")
        , ("testnet-magic", "42")
        , ("change-address", userAddress)
        , ("tx-in", txHash)
        , ("tx-out", scriptAddress <> " " <> show amount <> " lovelace")
        , ("tx-out-datum-hash-file", datumPath)
        , ("out-file", "out/tx.body")
        ]

signTransaction :: ProcessConfig () () ()
signTransaction =
  cardanoCli $
    ["transaction", "sign"]
      <> makeArgs
        [ ("tx-body-file", "out/tx.body")
        , ("signing-key-file", privateNetRoot <> "/addresses/user1.skey")
        , ("testnet-magic", testnetMagic)
        , ("out-file", "out/tx.signed")
        ]

submitTransaction :: ProcessConfig () () ()
submitTransaction =
  cardanoCli $
    ["transaction", "submit"]
      <> makeArgs
        [ ("testnet-magic", testnetMagic)
        , ("tx-file", "out/tx.signed")
        ]

buildConsumeTransaction ::
  Address ->
  TxHash ->
  String ->
  String ->
  ProcessConfig () () ()
buildConsumeTransaction userAddress scriptTxHash datumPath collateralTx =
  cardanoCli $
    ["transaction", "build"]
      <> makeArgs
        [ ("alonzo-era", "")
        , ("testnet-magic", testnetMagic)
        , ("change-address", userAddress)
        , ("tx-in", scriptTxHash)
        , ("tx-in-script-file", "script/validator.plutus")
        , ("tx-in-datum-file", datumPath)
        , ("tx-in-redeemer-file", "script/redeemer.json")
        , ("tx-in-collateral", collateralTx)
        , ("required-signer", privateNetRoot <> "/addresses/user1.skey")
        , ("protocol-params-file", "protocol-parameters.json")
        , ("out-file", "out/tx.body")
        ]
