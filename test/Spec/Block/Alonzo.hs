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
          "ff840db1ddc50f794c1c5d3ae7449d840cb46ed29238542d537b65be3e8a66f2"
      }

fixedBlockHeader :: Alonzo.BlockHeader
fixedBlockHeader =
  Alonzo.BlockHeader
    61633924
    "25221708259a71759422eb59ee19ae81bfbbdca2ab859a80b9d9904ad13be5e6"

fixedResultTip :: ResultTip
fixedResultTip =
  ResultTip
    { slot = 65281937
    , hash = "1a1bea726322dc9c44e214a2b5c63a1d7ebf5cdc67c2507f908e910b457769f0"
    , blockNo = 3757376
    }

fixedTransactions :: [Alonzo.Transaction]
fixedTransactions =
  [ Alonzo.Transaction
      mempty
      [ Alonzo.TxOut
          "addr_test1qp4w9qg8rn5jx72uyp5w8eu5ex7s0dtc7qpyyypnft239xkfy2ktyvf6alg\
          \5cvvd09z25d7zv8thtghgv0mfvj3w04asx2h36a"
          Nothing
      , Alonzo.TxOut
          "addr_test1qrasulh8u0j2wxm9dq7j839ld5c9727prudqvpe29qwdqwl3cmlde2cag9\
          \7r038pny27yc8rrpq7ut4u7je3t40fjqxqjjhj2q"
          Nothing
      ]
  , Alonzo.Transaction
      mempty
      [ Alonzo.TxOut
          "addr_test1qp4w9qg8rn5jx72uyp5w8eu5ex7s0dtc7qpyyypnft239xkfy2ktyvf6a\
          \lg5cvvd09z25d7zv8thtghgv0mfvj3w04asx2h36a"
          Nothing
      , Alonzo.TxOut
          "addr_test1qrx4w28mj9xzgpsrhlh2yg76nynagndg4slahmumvkde76nj5cgyvkz44\
          \8fn69s6xkezng6uujdl664v79wgs379x8xsgawpgf"
          Nothing
      ]
  , Alonzo.Transaction
      mempty
      [ Alonzo.TxOut
          "addr_test1qp4w9qg8rn5jx72uyp5w8eu5ex7s0dtc7qpyyypnft239xkfy2ktyvf6al\
          \g5cvvd09z25d7zv8thtghgv0mfvj3w04asx2h36a"
          Nothing
      , Alonzo.TxOut
          "addr_test1qpjcmulxjzhr2e6q2pkxltf7r9e7zfjv80dua8v230wr3huwjylwgwxfn9\
          \ja3vvt64qp3rxskgs6695egjs35rhuhd6s4z2k3j"
          Nothing
      ]
  ]

fixedRawTransactions :: [Alonzo.RawTransaction]
fixedRawTransactions =
  [ Alonzo.RawTransaction
      { txId =
          "b8da9b46763f2762d23be2131d456406e1f0d7a35c0ee07c395e29c208c4335c"
      , rawTx = "hKQAgYJYIO0B3YMixM1w2NVoVIACb5B9tcWVUKJt2M6vJ7LTKhLtAQGCglg5AGrigQcc6SN5XCBo4+eUyb0HtXjwAkIQM0rVEprJIqyyMTrv0UwxjXlEqjfCYdd1ouhj9pZKLn17ghoAxl1AoIJYOQD7Dn7n4+SnG2VoPSPEv20wXyvBHxoGByooHNA78cb+3KsdQXw3xOGZFeJg4xhB7i689LMV1emQDBo327mDAhoAApqZB1ggOT84WXpP3HQJn0jPAwgT9od+f/XcleqI6Jfg+vnVf5KhAIGCWCCwBlrw110BBVbBWZ+kYhNv4TtIfUXbxARJu6OGzzR/x1hANWj/1Lgic+LCgRb051S0vFI6G9uBRLckbhdkjVsC0DqbBNb6296Xbh3gxB0a0FHCeG+lk1lNKiUGtDKf49NyB/WhAFTUL+HpTM4VklvT4oBmKS/pavshAA=="
      }
  , Alonzo.RawTransaction
      { txId =
          "4a9e9434235003653d3a697ff8718d073633d61ede523d3cc5473edd0882becd"
      , rawTx = "hKQAgYJYIKnMHHtc7TKLhCbLMItpUUX9mqsq+u0jDea4tlcF9y2fAQGCglg5AGrigQcc6SN5XCBo4+eUyb0HtXjwAkIQM0rVEprJIqyyMTrv0UwxjXlEqjfCYdd1ouhj9pZKLn17ghoAtxsAoIJYOQDNVyj7kUwkBgO/7qIj2pkn1E2orD/b75tlm59qcqYQRlhVqdM9Fho1simjXOSb/Wqs8VyIR8UxzRoc2auVAhoAApqZB1ggivErf4ZpKcTw/B7FsgQM8WufHy0CGFYDNz2iL1Zu3hChAIGCWCAiUmOzluwu+5E0/lbJ4teGH4OY6Q7PpKI1A+SbVx1EUlhANDWuLl8EWSjXH3jOTrLRM0S0bA5nL2j0Ny7bIvuuXNVV++CW0BPLMGjNu0LrvnRw9vKq62g6datFp2fXcMWcAvWhAFQefUl8y9rhiu5tUYPJSXOqbR/QrQ=="
      }
  , Alonzo.RawTransaction
      { txId =
          "0e26996e31f6766711a108aa9bc975f4ff8350d221bc9bdda613b7b31a9eec2a"
      , rawTx = "hKQAgYJYIKP1sr0xVamZEllYwxLMi84sPhEZax3yfz6ZxdQ1ZeZnAQGCglg5AGrigQcc6SN5XCBo4+eUyb0HtXjwAkIQM0rVEprJIqyyMTrv0UwxjXlEqjfCYdd1ouhj9pZKLn17ghoAtxsAoIJYOQBljfPmkK41Z0BQbG+tPhlz4SZMO9vOnYqL3DjfjpE+5DjJmWXYsYvVQBiM0LIhrRaZRKEaDvy7dRoXVVIeAhoAApqZB1ggKwMKJlBXGr1rwO1yjY0GM2LSIoHUyVrbLd5yZPcKkwWhAIGCWCDhPc0s0402+K08mbCdojFfn47XCvWdh5EmCAoGf+dYYlhA9Rz4/YUS6Gc862zXnBtjBnCGzSlM+b3Y4FhdjAbam4TAfrjAG48f7b0toNSaJsHivYnGxqG52kt1d8+ggafABPWhAFTPMlGV6fGObFwpNNpRJZjnzQIOxQ=="
      }
  ]
