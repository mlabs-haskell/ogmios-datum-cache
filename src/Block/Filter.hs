module Block.Filter (DatumFilter (..), runDatumFilter) where

import Data.Text (Text)
import GHC.Exts (toList)
import Data.Aeson
import Data.Maybe (mapMaybe)

import Block.Types

data DatumFilter
    = ConstFilter Bool
    | AnyFilter [DatumFilter]
    | AllFilter [DatumFilter]
    | DatumHashFilter Text
    | AddressFilter Text
  deriving stock Show

instance FromJSON DatumFilter where
    parseJSON = withObject "DatumFilter" $ \o -> do
        case toList o of
            [("const", Bool c)] -> pure $ ConstFilter c
            [("any", filters')] -> do
              filters <- parseJSON filters'
              pure $ AnyFilter filters
            [("all", filters')] -> do
              filters <- parseJSON filters'
              pure $ AllFilter filters
            [("hash", String h)] -> pure $ DatumHashFilter h
            [("address", String a)] -> pure $ AddressFilter a
            _ -> fail "Failed parsing DatumFilter"

-- TODO
runDatumFilter :: DatumFilter -> AlonzoTransaction -> (Text, Text) -> Bool
runDatumFilter (ConstFilter b) _ _ = b
runDatumFilter (AnyFilter filters) tx datum = any (\f -> runDatumFilter f tx datum) filters
runDatumFilter (AllFilter filters) tx datum = all (\f -> runDatumFilter f tx datum) filters
runDatumFilter (DatumHashFilter expectedHash) _ (actualHash, _) = expectedHash == actualHash
runDatumFilter (AddressFilter expectedAddress) tx (actualHash, _) =
  let hashes = mapMaybe datumHash $ filter ((== expectedAddress) . address) $ outputs tx
   in actualHash `elem` hashes
