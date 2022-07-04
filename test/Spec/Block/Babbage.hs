module Spec.Block.Babbage (example) where

import Block.Types (
  RequestNextResult (RollForward),
  ResultTip (ResultTip, blockNo, hash, slot),
  SomeBlock (BabbageBlock),
 )
import Block.Types.Babbage qualified as Babbage

example :: RequestNextResult
example =
  RollForward block resultTip

block :: Babbage.Block
block =
  BabbageBlock $
    Babbage.Block
      { body =
          []
      , rawTransactions = []
      , header = fixedBlockHeader
      , headerHash = ""
      }

fixedBlockHeader :: Babbage.BlockHeader
fixedBlockHeader = Babbage.BlockHeader 1 ""

resultTip :: ResultTip
resultTip =
  ResultTip
    { slot = 12
    , hash = "c248757d390181c517a5beadc9c3fe64bf821d3e889a963fc717003ec248757d"
    , blockNo = 18446744073709552000
    }

fixedOutputs :: [TxOut]
fixedOutputs = [TxOut "addr_test1qz66ue36465w2qq40005h2hadad6pnjht8mu6sgplsfj74qdjnshguewlx4ww0eet26y2pal4xpav5prcydf28cvxtjqx46x7f" Nothing Nothing]

fixedDatums :: Map Text Text
fixedDatums = Map.fromList [("property1", "string"), ("property2", "string")]
