module MyAES where

import qualified Data.ByteString as B
import Data.Word
import Data.Bits hiding (rotate)
import MyCryptoUtil

data Direction' = Encrypt' | Decrypt'

wordToInt = fromIntegral . toInteger

isEncrypt :: Direction' -> Bool
isEncrypt Encrypt' = True
isEncrypt _ = False


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

-- For 16 bytes key, the number of expansion cycles is 10 (24 bytes = 12 cycles, 32 bytes = 14 cycles)
{-
key' = decodeHexString "af7f67980cb7add647d9e8590f1571c9"
key'' = decodeHexString "000102030405060708090A0B0C0D0E0F"

aesKeyExpansion key = take numberOfCycles $ iterate (\(k, kR) -> nextKey (k, kR)) (key, 1)
  where
    numberOfCycles
      | l == 16 = 10
      | l == 24 = 12
      | l == 32 = 14
      | otherwise = 0
      where
        l = B.length key
    nextKey (pKey, keyRound) = (B.concat [w7, w6, w5, w4], keyRound + 1)
      where
        w7 = zipBytesWith xor w6 w3
        w6 = zipBytesWith xor w5 w2
        w5 = zipBytesWith xor w4 w1
        w4 = zipBytesWith xor z1 w0
        z1 = zipBytesWith xor y1 (rcon keyRound)
        y1 = B.pack $ map (sBoxAESLookup Encrypt') x1
        x1 = B.unpack $ rotate (head keyWords) 1
        keyWords = splitEvery 4 pKey
        (w3, w2, w1, w0) = (keyWords !! 0, keyWords !! 1, keyWords !! 2, keyWords !! 3)
        rcon n = B.pack $ (take 1 $ drop (n - 1) [1, 2, 4, 8, 16, 32, 64, 128, 27, 54, 108, 216, 171, 77, 154]::[Word8]) ++ [0, 0, 0]
-}
