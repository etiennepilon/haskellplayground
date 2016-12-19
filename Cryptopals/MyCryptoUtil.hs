module MyCryptoUtil where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import Data.Bits
import Data.Word

data Direction' = Encrypt' | Decrypt'

isEncrypt :: Direction' -> Bool
isEncrypt Encrypt' = True
isEncrypt _ = False

wordToInt = fromIntegral . toInteger

zipBytesWith :: (Word8 -> Word8 -> Word8) -> B.ByteString -> B.ByteString -> B.ByteString
zipBytesWith f a b = B.pack $ B.zipWith f a b 

encodeBytesToBase64 :: B.ByteString -> B.ByteString
encodeBytesToBase64 = B64.encode

encodeStringToHex :: B.ByteString -> B.ByteString
encodeStringToHex = B16.encode

decodeBase64String :: String -> B.ByteString
decodeBase64String = B64.decodeLenient . C8.pack

decodeHexString :: String -> B.ByteString
decodeHexString = fst . B16.decode . C8.pack


stringToBytes :: String -> B.ByteString
stringToBytes = C8.pack

hammingDistance :: B.ByteString -> B.ByteString -> Int
hammingDistance a b = sum $ map popCount $ B.zipWith xor a b

splitEvery :: Int -> B.ByteString -> [B.ByteString]
splitEvery n = takeWhile (not . B.null) . map (B.take n) . iterate (B.drop n)

combinations :: [a] -> [(a, a)]
combinations xs
  | length xs == 1 = []
  | otherwise = [(x, y)| x <- [head xs], y <- (drop 1 xs)] ++ combinations (drop 1 xs)

pkcs7Padding :: B.ByteString -> Int -> B.ByteString
pkcs7Padding xs l = B.concat $ [xs, (B.replicate padLength (fromIntegral padLength))]
  where
    padLength = l - (B.length xs)
    

numberOfCycles :: B.ByteString -> Int
numberOfCycles key 
  | l == 16 = 10
  | l == 24 = 12
  | l == 32 = 14
  | otherwise = 0
  where
    l = B.length key

testKey = decodeHexString "000102030405060708090A0B0C0D0E0F"
-- AES Encryption algorithm
-- Key Expansion
-- Keys are taken by chunks of 4 bytes with MSB being the last byte (w3, w2, w1, w0)
aeskeyExpansion key = nextKey (key, 1)
  where
    nextKey (previousKey, keyRound) = B.concat [w3, w2, w1, w0]
      where
        w3 = bXor w2 (pKeyChunks !! 3)
        w2 = bXor w1 (pKeyChunks !! 2)
        w1 = bXor w0 (pKeyChunks !! 1)
        w0 = bXor (head pKeyChunks) z1
        z1 = bXor (rCon keyRound) y1 
        y1 = B.pack $ map (sBoxAESLookup Encrypt') x1
        x1 = B.unpack $ rotWord (last pKeyChunks)
        -- (w0, w1, w2, w3)
        pKeyChunks = reverse $ splitEvery 4 previousKey
    -- left shift of one
    rotWord w = rotateBytestringOfNBytes w 1
    rCon n = B.pack $ (take 1 $ drop (n - 1) [2, 4, 8, 16, 32, 64, 128, 27, 54, 108, 216, 171, 77, 154]::[Word8]) ++ [0, 0, 0]
    bXor a b = B.pack $ B.zipWith xor a b
{-
--AESkeyExpansion :: B.ByteString -> [B.ByteString]
aeskey16Expansion key = nextKey (key, 1)
  where
    nextKey (previousKey, keyRoundNumber) = concat $ w0:(map wn [1..((length pKeyChunks) - 1)])
      where
        wn n = B.zipWith xor (pKeyChunks !! n) (B.pack w0)
        w0 = B.zipWith xor (head pKeyChunks) $ B.pack $ B.zipWith xor (rotateBytestringOfNBytes (last pKeyChunks) (-1)) (rconLookup keyRoundNumber)
        pKeyChunks = reverse $ splitEvery 4 previousKey 
    keyRounds = numberOfCycles key
    rCon n = B.pack $ (take 1 $ drop (n - 1) [2, 4, 8, 16, 32, 64, 128, 27, 54, 108, 216, 171, 77, 154]::[Word8]) ++ [0, 0, 0]
-}
rotateBytestringOfNBytes :: B.ByteString -> Int -> B.ByteString
rotateBytestringOfNBytes str n = B.concat [B.drop rotFactor str, B.take rotFactor str]
  where
    rotFactor
      | n >= 0 = n `mod` l
      | otherwise = (l - ((-n) `mod` l))
    l = B.length str

sBoxAESLookup :: Direction' -> Word8 -> Word8
sBoxAESLookup d byte
  | isEncrypt d = lookup sBoxAESEncryptionTable
  | otherwise = lookup sBoxAESDecryptionTable
  where
    columnNum = (.&.) byte 15
    rowNum = shift ((.&.) byte 240) (-4)
    lookup xs = B.head $ B.drop (wordToInt columnNum) $ head $ drop (wordToInt rowNum) xs
    sBoxAESEncryptionTable = splitEvery 16 $ decodeHexString "637C777BF26B6FC53001672BFED7AB76CA82C97DFA5947F0ADD4A2AF9CA472C0B7FD9326363FF7CC34A5E5F171D8311504C723C31896059A071280E2EB27B27509832C1A1B6E5AA0523BD6B329E32F8453D100ED20FCB15B6ACBBE394A4C58CFD0EFAAFB434D338545F9027F503C9FA851A3408F929D38F5BCB6DA2110FFF3D2CD0C13EC5F974417C4A77E3D645D197360814FDC222A908846EEB814DE5E0BDBE0323A0A4906245CC2D3AC629195E479E7C8376D8DD54EA96C56F4EA657AAE08BA78252E1CA6B4C6E8DD741F4BBD8B8A703EB5664803F60E613557B986C11D9EE1F8981169D98E949B1E87E9CE5528DF8CA1890DBFE6426841992D0FB054BB16"
    sBoxAESDecryptionTable = splitEvery 16 $ decodeHexString "52096AD53036A538BF40A39E81F3D7FB7CE339829B2FFF87348E4344C4DEE9CB547B9432A6C2233DEE4C950B42FAC34E082EA16628D924B2765BA2496D8BD12572F8F66486689816D4A45CCC5D65B6926C704850FDEDB9DA5E154657A78D9D8490D8AB008CBCD30AF7E45805B8B34506D02C1E8FCA3F0F02C1AFBD0301138A6B3A9111414F67DCEA97F2CFCEF0B4E67396AC7422E7AD3585E2F937E81C75DF6E47F11A711D29C5896FB7620EAA18BE1BFC563E4BC6D279209ADBC0FE78CD5AF41FDDA8338807C731B11210592780EC5F60517FA919B54A0D2DE57A9F93C99CEFA0E03B4DAE2AF5B0C8EBBB3C83539961172B047EBA77D626E169146355210C7D"


{-
 - 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F
 - D6 AA 74 FD D2 AF 72 FA DA A6 78 F1 D6 AB 76 FE
 - B6 92 CF 0B 64 3D BD F1 BE 9B C5 00 68 30 B3 FE
 - B6 FF 74 4E D2 C2 C9 BF 6C 59 0C BF 04 69 BF 41
 - 47 F7 F7 BC 95 35 3E 03 F9 6C 32 BC FD 05 8D FD
 - 3C AA A3 E8 A9 9F 9D EB 50 F3 AF 57 AD F6 22 AA
 - 5E 39 0F 7D F7 A6 92 96 A7 55 3D C1 0A A3 1F 6B
 - 14 F9 70 1A E3 5F E2 8C 44 0A DF 4D 4E A9 C0 26
 - 47 43 87 35 A4 1C 65 B9 E0 16 BA F4 AE BF 7A D2
 - 54 99 32 D1 F0 85 57 68 10 93 ED 9C BE 2C 97 4E
 - 13 11 1D 7F E3 94 4A 17 F3 07 A7 8B 4D 2B 30 C5
 -
-}
