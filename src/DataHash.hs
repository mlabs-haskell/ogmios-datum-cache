module DataHash (DataHash (..)) where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON (toJSON))
import Data.Text qualified as Text
import Servant.API.Generic (Generic)

newtype DataHash = DataHash {dataHash :: Text.Text}
  deriving stock (Generic, Show, Eq)
  deriving newtype (Ord, FromJSONKey)
  deriving anyclass (FromJSON)

instance ToJSON DataHash where
  toJSON = toJSON . dataHash
