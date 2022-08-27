{-# LANGUAGE DeriveGeneric #-}

-- Adapted from
-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-core/html/src/PlutusCore.Data.html#line-58
module Spec.Types (PlutusData (Constr, Map, List, I, B)) where

import Codec.CBOR.Encoding (Encoding)
import Codec.CBOR.Encoding qualified as CBOR
import Codec.Serialise (Serialise (encode))

--import Data.Aeson (ToJSON, (.=))
--import Data.Aeson qualified as Aeson
--import Data.Aeson.Encoding (list)
import Data.Bits (shiftR)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)

data PlutusData
  = Constr Integer [PlutusData]
  | Map [(PlutusData, PlutusData)]
  | List [PlutusData]
  | I Integer
  | B ByteString
  deriving stock (Show, Eq, Ord, Generic)

--instance ToJSON PlutusData where
--  toEncoding (Constr number fields) =
--    Aeson.pairs ("constr" .= number <> "fields" .= Aeson.toEncodingList fields)
--  toEncoding (Map elems) = Aeson.pairs ("map" .= list (\(k, v) -> Aeson.pairs ("key" .= Aeson.toEncoding k <> "value" .= Aeson.encode v)) elems)
--  toEncoding (List elems) = Aeson.toEncoding elems
--  toEncoding (I bi) = Aeson.toEncoding bi
--  toEncoding (B ba) = Aeson.toEncoding ba

--instance ToJSON PlutusData where
--  encode (Constr constr fields) = Aeson.encode
--    { "constr": Aeson.encode constr
--    , "fields": Aeson.encode fields
--    }
--  encode (Map elems) = Aeson.encode
--    { "map": Aeson.encode $ map
--        ( \(k /\ v) ->
--            { "key": Aeson.encode k
--            , "value": Aeson.encode v
--            }
--        )
--        elems
--    }
--  encode (List elems) = Aeson.encode elems
--  encode (Integer bi) = Aeson.encode bi
--  encode (Bytes ba) = Aeson.encode ba

instance Serialise PlutusData where
  -- See Note [Encoding via Term]
  encode = encodeData

-- decode = decodeData

-- | Turn Data into a CBOR Term.
encodeData :: PlutusData -> Encoding
encodeData = \case
  -- See Note [CBOR alternative tags]
  Constr i ds | 0 <= i && i < 7 -> CBOR.encodeTag (fromIntegral (121 + i)) <> encode ds
  Constr i ds | 7 <= i && i < 128 -> CBOR.encodeTag (fromIntegral (1280 + (i - 7))) <> encode ds
  Constr i ds
    | otherwise ->
      let tagEncoding =
            if fromIntegral (minBound @Word64) <= i && i <= fromIntegral (maxBound @Word64)
              then CBOR.encodeWord64 (fromIntegral i)
              else -- This is a "correct"-ish encoding of the tag, but it will *not* deserialise, since we insist on a
              -- 'Word64' when we deserialise. So this is really a "soft" failure, without using 'error' or something.
                CBOR.encodeInteger i
       in CBOR.encodeTag 102 <> CBOR.encodeListLen 2 <> tagEncoding <> encode ds
  Map es -> CBOR.encodeMapLen (fromIntegral $ length es) <> mconcat [encode t <> encode t' | (t, t') <- es]
  List ds -> encode ds
  I i -> encodeInteger i
  B b -> encodeBs b

-- Logic for choosing encoding borrowed from Codec.CBOR.Write

-- | Given an integer, create a 'CBOR.Term' that encodes it, following our size restrictions.
encodeInteger :: Integer -> Encoding
-- If it fits in a Word64, then it's less than 64 bytes for sure, and we can just send it off
-- as a normal integer for cborg to deal with
encodeInteger i
  | i >= 0, i <= fromIntegral (maxBound :: Word64) = CBOR.encodeInteger i
  | i < 0, i >= -1 - fromIntegral (maxBound :: Word64) = CBOR.encodeInteger i
-- Otherwise, it would be encoded as a bignum anyway, so we manually do the bignum
-- encoding with a bytestring inside, and since we use bsToTerm, that bytestring will
-- get chunked up if it's too big.
-- See Note [Evading the 64-byte limit]
encodeInteger i | i >= 0 = CBOR.encodeTag 2 <> encodeBs (integerToBytes i)
encodeInteger i | otherwise = CBOR.encodeTag 3 <> encodeBs (integerToBytes (-1 - i))

-- | Given an bytestring, create a 'CBOR.Term' that encodes it, following our size restrictions.
encodeBs :: ByteString -> Encoding
encodeBs b | ByteString.length b <= 64 = CBOR.encodeBytes b
-- It's a bit tricky to get cborg to emit an indefinite-length bytestring with chunks that we control,
-- so we encode it manually
-- See Note [Evading the 64-byte limit]
encodeBs b = CBOR.encodeBytesIndef <> foldMap encode (to64ByteChunks b) <> CBOR.encodeBreak

-- Taken exactly from Codec.CBOR.Write
integerToBytes :: Integer -> ByteString.ByteString
integerToBytes n0
  | n0 == 0 = ByteString.pack [0]
  | otherwise = ByteString.pack (reverse (go n0))
  where
    go n
      | n == 0 = []
      | otherwise = narrow n : go (n `shiftR` 8)

    narrow :: Integer -> Word8
    narrow = fromIntegral

-- | Turns a 'BS.ByteString' into a list of <=64 byte chunks.
to64ByteChunks :: ByteString.ByteString -> [ByteString.ByteString]
to64ByteChunks b
  | ByteString.length b > 64 =
    let (chunk, rest) = ByteString.splitAt 64 b
     in chunk : to64ByteChunks rest
to64ByteChunks b = [b]
