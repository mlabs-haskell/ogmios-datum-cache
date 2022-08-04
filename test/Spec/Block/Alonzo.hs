module Spec.Block.Alonzo (example) where

import Block.Types (
  RequestNextResult (RollForward),
  ResultTip (ResultTip, blockNo, hash, slot),
  SomeBlock (AlonzoBlock),
 )
import Block.Types.Alonzo qualified as Alonzo

example :: RequestNextResult
example =
  RollForward fixedBlock fixedResultTip

fixedBlock :: SomeBlock
fixedBlock =
  AlonzoBlock $
    Alonzo.Block
      { body = fixedTransactions
      , rawTransactions = fixedRawTransactions
      , header = fixedBlockHeader
      , headerHash =
          "4db80fd70a8859b7e706a23239210eab53c130875ff6bae729a531e6564aaf72"
      }

fixedBlockHeader :: Alonzo.BlockHeader
fixedBlockHeader =
  Alonzo.BlockHeader
    61625683
    "595a69c565a8aeb0195cd7aaf8abf7a145530b83fa0bcb59fd57bfe8bb19b08b"

fixedResultTip :: ResultTip
fixedResultTip =
  ResultTip
    { slot = 62284316
    , hash = "30b253b54dbaf6fbacef0f3cb5c46dde178044406a7022672d8ee9c94034649a"
    , blockNo = 3673647
    }

fixedTransactions :: [Alonzo.Transaction]
fixedTransactions =
  [ Alonzo.Transaction
      mempty
      [ Alonzo.TxOut
          "addr_test1qp4w9qg8rn5jx72uyp5w8eu5ex7s0dtc7qpyyypnft239xkfy2ktyvf6a\
          \lg5cvvd09z25d7zv8thtghgv0mfvj3w04asx2h36a"
          Nothing
      , Alonzo.TxOut
          "addr_test1qp0k2gzfuq8t2jnrlpursl3x45944aa86f97llftwzrdha0rzqd449uew\
          \rc0y73ktgm2v5tqx0q45zt2quzxjxnll89su8nae7"
          Nothing
      ]
  , Alonzo.Transaction
      mempty
      [ Alonzo.TxOut
          "addr_test1qp4w9qg8rn5jx72uyp5w8eu5ex7s0dtc7qpyyypnft239xkfy2ktyvf6a\
          \lg5cvvd09z25d7zv8thtghgv0mfvj3w04asx2h36a"
          Nothing
      , Alonzo.TxOut
          "addr_test1qq5wl8jrkxvzdmltptnka044h7cx3ynj9d7sajkj6nfx5xxapxsnzns0y\
          \fvn6dcfwkyndpy6wcnnpy25f3h4w9dxqmusg85hd2"
          Nothing
      ]
  , Alonzo.Transaction
      mempty
      [ Alonzo.TxOut
          "addr_test1qp4w9qg8rn5jx72uyp5w8eu5ex7s0dtc7qpyyypnft239xkfy2ktyvf6a\
          \lg5cvvd09z25d7zv8thtghgv0mfvj3w04asx2h36a"
          Nothing
      , Alonzo.TxOut
          "addr_test1qr35s5q9rupphqhyk27ssej2s2vsghqae6lqzfxwlh0mn9dz3phanmndx\
          \5lf3ppw7n3w5el430p5s4p5350kqa3ayhzq6vkf5c"
          Nothing
      ]
  ]

fixedRawTransactions :: [Alonzo.RawTransaction]
fixedRawTransactions =
  [ Alonzo.RawTransaction
      { txId =
          "63d086e9d793350e5a7c1370928b640a2648fdff363b2a72eb1a028e4c97142c"
      , rawTx = "test"
      }
  , Alonzo.RawTransaction
      { txId =
          "b988256050c838c18a73b9792a64e3e7ae80da6b0078c47cedd1d20e241896d6"
      , rawTx = "test"
      }
  , Alonzo.RawTransaction
      { txId =
          "da32cad61863bcb38ceda40db55fc4fc47c49d6ca777314c4ba48c9b7cce2451"
      , rawTx = "test"
      }
  ]
