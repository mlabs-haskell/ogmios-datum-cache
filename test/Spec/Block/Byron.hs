module Spec.Block.Byron (example) where

import Block.Types (
  RequestNextResult (RollForward),
  ResultTip (ResultTip, blockNo, hash, slot),
  SomeBlock (ByronBlock),
 )
import Block.Types.Byron qualified as Byron

example :: RequestNextResult
example =
  RollForward fixedBlock fixedResultTip

fixedBlock :: SomeBlock
fixedBlock =
  ByronBlock $
    Byron.Block
      { slot = 341860
      , headerHash =
          "085cb94fcf67deb81b0d044171df4eeb99c51635bcd24bdf70723bc2b4258e11"
      }

fixedResultTip :: ResultTip
fixedResultTip =
  ResultTip
    { slot = 62501845
    , hash = "ae07ffcf32670911486f65cc625ebfb339db09a58bbe1d6eb961988bc97516fb"
    , blockNo = 3680348
    }
