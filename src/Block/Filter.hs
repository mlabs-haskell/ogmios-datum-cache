module Block.Filter (DatumFilter (..), defaultDatumFilter, runDatumFilter, TxFilter, runFilter, txFitlerFromDatumFilter) where

import Data.Aeson (FromJSON (parseJSON), Value (Bool, String), withObject)
import Data.Default (Default (def))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import GHC.Exts (toList)

import Block.Types (AlonzoTransaction (outputs), TxOut (address, datumHash))

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

runDatumFilter :: DatumFilter -> AlonzoTransaction -> (Text, Text) -> Bool
runDatumFilter (ConstFilter b) _ _ = b
runDatumFilter (AnyFilter filters) tx datum =
  any (\f -> runDatumFilter f tx datum) filters
runDatumFilter (AllFilter filters) tx datum =
  all (\f -> runDatumFilter f tx datum) filters
runDatumFilter (DatumHashFilter expectedHash) _ (actualHash, _) =
  expectedHash == actualHash
runDatumFilter (AddressFilter expectedAddress) tx (actualHash, _) =
  let hashes =
        mapMaybe datumHash $
          filter ((== expectedAddress) . address) $
            outputs tx
   in actualHash `elem` hashes

newtype TxFilter = TxFilter ()

runFilter :: TxFilter -> AlonzoTransaction -> Bool
runFilter = error ""

txFitlerFromDatumFilter :: DatumFilter -> TxFilter
txFitlerFromDatumFilter = error ""