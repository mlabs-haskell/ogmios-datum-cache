module Block.Filter (DatumFilter (..), defaultDatumFilter, runDatumFilter) where

import Data.Aeson (FromJSON (parseJSON), Value (Bool, String), withObject)
import Data.Default (Default (def))
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Text (Text)
import GHC.Exts (toList)

import Block.Types (AlonzoTransaction (outputs), TxOut (address, datumHash))
import Filters (Filter, Semiring (srconcat))

data DatumFilter
  = ConstFilter Bool
  | AnyFilter [DatumFilter]
  | AllFilter [DatumFilter]
  | DatumHashFilter Text
  | AddressFilter Text
  deriving stock (Show, Eq)

-- | Filter that accepts all datums
defaultDatumFilter :: DatumFilter
defaultDatumFilter = ConstFilter True

instance Default DatumFilter where
  def = defaultDatumFilter

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

createDatumFilter :: DatumFilter -> Filter (AlonzoTransaction, (Text, Text))
createDatumFilter (ConstFilter b) = const b
createDatumFilter (AnyFilter list) = srconcat (map createDatumFilter list)
createDatumFilter (AllFilter list) = mconcat (map createDatumFilter list)
createDatumFilter (DatumHashFilter expectedHash) = \(_, (actualHash, _)) -> expectedHash == actualHash
createDatumFilter (AddressFilter expectedAddress) = \(tx, (actualHash, _)) ->
  let utxos = outputs tx
      overall a b = srconcat (((==b) . address) <> (Just a==) . datumHash <$> utxos)
   in overall actualHash expectedAddress

runDatumFilter :: DatumFilter -> AlonzoTransaction -> (Text, Text) -> Bool
runDatumFilter d a t = createDatumFilter d (a, t)
