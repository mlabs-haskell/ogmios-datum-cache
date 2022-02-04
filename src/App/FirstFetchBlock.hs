module App.FirstFetchBlock (FirstFetchBlock (..)) where

import Data.Text (Text)

data FirstFetchBlock = FirstFetchBlock
    { blockSlot :: Integer
    , blockId :: Text
    }
