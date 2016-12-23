module MyCryptoUtil where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import Data.Bits
import Data.Word
import System.Random

-- Encoding functions
--
encodeBytesToBase64 :: B.ByteString -> B.ByteString
encodeBytesToBase64 = B64.encode

encodeStringToHex :: B.ByteString -> B.ByteString
encodeStringToHex = B16.encode

-- Decoding functions
--
decodeBase64String :: String -> B.ByteString
decodeBase64String = B64.decodeLenient . C8.pack

decodeHexString :: String -> B.ByteString
decodeHexString = fst . B16.decode . C8.pack

stringToBytes :: String -> B.ByteString
stringToBytes = C8.pack

-- ByteString Util
--
bytesToInt :: (Integral a) => a -> Int
bytesToInt = fromIntegral . toInteger

zipBytesWith :: (Word8 -> Word8 -> Word8) -> B.ByteString -> B.ByteString -> B.ByteString
zipBytesWith f a b = B.pack $ B.zipWith f a b 

splitEvery :: Int -> B.ByteString -> [B.ByteString]
splitEvery n = takeWhile (not . B.null) . map (B.take n) . iterate (B.drop n)


rotate :: B.ByteString -> Int -> B.ByteString
rotate str n = B.concat [B.drop rotFactor str, B.take rotFactor str]
  where
    rotFactor 
      | n >= 0 = n `mod` l
      | otherwise = (l - ((-n) `mod` l))
    l = B.length str

-- Crypto Util
--
hammingDistance :: B.ByteString -> B.ByteString -> Int
hammingDistance a b = sum $ map popCount $ B.zipWith xor a b

singleByteXor :: B.ByteString -> Word8 -> B.ByteString
singleByteXor msg n = B.map (xor n) msg

pkcs7padding :: B.ByteString -> Int -> B.ByteString
pkcs7padding xs l = B.concat $ [xs, (B.replicate padLength (fromIntegral padLength))]
  where
    padLength = l - (B.length xs)

-- Strict ByteString can't cycle multiple times. Thus, it's necesary to convert to string
repeatedXOR :: B.ByteString -> B.ByteString -> B.ByteString
repeatedXOR msg key = B.pack $ zipWith xor (B.unpack msg) (cycle $ B.unpack key)
-- Others
-- 
combinations :: [a] -> [(a, a)]
combinations xs
  | length xs == 1 = []
  | otherwise = [(x, y)| x <- [head xs], y <- (drop 1 xs)] ++ combinations (drop 1 xs)

randBytes :: Int -> IO B.ByteString
randBytes n = newStdGen >>= (\x->return $ B.pack $ take n $ randomRs (0, 255) x)
