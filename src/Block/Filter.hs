module Block.Filter (DatumFilter (..), defaultDatumFilter, runDatumFilter) where

import Data.Aeson (FromJSON (parseJSON), Value (Bool, String), withObject)
import Data.Default (Default (def))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import GHC.Exts (toList)

import Block.Types (SomeTransaction (AlonzoTransaction, BabbageTransaction))
import DataHash (DataHash (DataHash))

data DatumFilter
  = ConstFilter Bool
  | AnyFilter [DatumFilter]
  | AllFilter [DatumFilter]
  | DatumHashFilter DataHash
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
      [("hash", String h)] -> (pure . DatumHashFilter . DataHash) h
      [("address", String a)] -> pure $ AddressFilter a
      _ -> fail "Failed parsing DatumFilter"

runDatumFilter :: DatumFilter -> SomeTransaction -> (DataHash, Text) -> Bool
runDatumFilter (ConstFilter b) _ _ = b
runDatumFilter (AnyFilter filters) tx datum =
  any (\f -> runDatumFilter f tx datum) filters
runDatumFilter (AllFilter filters) tx datum =
  all (\f -> runDatumFilter f tx datum) filters
runDatumFilter (DatumHashFilter expectedHash) _ (actualHash, _) =
  expectedHash == actualHash
runDatumFilter (AddressFilter expectedAddress) (AlonzoTransaction tx') (actualHash, _) =
  let hashes =
        mapMaybe (.datumHash)
          . filter (\tx -> tx.address == expectedAddress)
          $ tx'.outputs
   in actualHash `elem` hashes
runDatumFilter (AddressFilter expectedAddress) (BabbageTransaction tx') (actualHash, _) =
  let hashes =
        mapMaybe (.datumHash)
          . filter (\tx -> tx.address == expectedAddress)
          $ tx'.outputs
   in actualHash `elem` hashes
