module Block.Types.Byron (
  Block (..),
) where

-- This module is intended to be imported qualified

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Int (Int64)
import Data.Text (Text)

data Block = Block
  { slot :: Int64
  , headerHash :: Text
  }
  deriving stock (Eq, Show)

instance FromJSON Block where
  parseJSON = withObject "Block" $ \o -> do
    headerHash <- o .: "hash"
    header <- o .: "header"
    slot <- header .: "slot"
    pure $ Block slot headerHash
