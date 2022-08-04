module Spec.Block.Babbage (example) where

import Data.Map qualified as Map
import Data.Text qualified as Text

import Block.Types (
  RequestNextResult (RollForward),
  ResultTip (ResultTip, blockNo, hash, slot),
  SomeBlock (BabbageBlock),
 )
import Block.Types.Babbage qualified as Babbage
import DataHash (DataHash (DataHash))

example :: RequestNextResult
example =
  RollForward fixedBlock fixedResultTip

fixedBlock :: SomeBlock
fixedBlock =
  BabbageBlock $
    Babbage.Block
      { body = fixedTransactions
      , rawTransactions = fixedRawTransactions
      , header = fixedBlockHeader
      , headerHash =
          "c248757d390181c517a5beadc9c3fe64bf821d3e889a963fc717003ec248757d"
      }

fixedBlockHeader :: Babbage.BlockHeader
fixedBlockHeader =
  Babbage.BlockHeader
    13
    "c248757d390181c517a5beadc9c3fe64bf821d3e889a963fc717003ec248757d"

fixedResultTip :: ResultTip
fixedResultTip =
  ResultTip
    { slot = 12
    , hash = "c248757d390181c517a5beadc9c3fe64bf821d3e889a963fc717003ec248757d"
    , blockNo = 18446744073709552000
    }

fixedTransactions :: [Babbage.Transaction]
fixedTransactions = [Babbage.Transaction fixedDatums fixedOutputs]

fixedOutputs :: [Babbage.TxOut]
fixedOutputs =
  [ Babbage.TxOut
      "addr_test1qz66ue36465w2qq40005h2hadad6pnjht8mu6sgplsfj74qdjnshguewlx4ww\
      \0eet26y2pal4xpav5prcydf28cvxtjqx46x7f"
      Nothing
      Nothing
  ]

fixedDatums :: Map.Map DataHash Text.Text
fixedDatums = Map.fromList [(DataHash "property1", "string"), (DataHash "property2", "string")]

fixedRawTransactions :: [Babbage.RawTransaction]
fixedRawTransactions =
  [ Babbage.RawTransaction
      { txId =
          "c248757d390181c517a5beadc9c3fe64bf821d3e889a963fc717003ec248757d"
      , rawTx = "test"
      }
  ]
