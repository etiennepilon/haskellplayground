module Set1 where

import MyCryptoUtil
import qualified Data.ByteString as B
import Data.Bits
import qualified Data.List as L

challenge1 :: B.ByteString
challenge1 = encodeBytesToBase64 $ decodeHexString hexStr
  where
    hexStr = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

challenge2 :: B.ByteString
challenge2 = encodeStringToHex $ zipBytesWith xor s1 s2
  where
    s1 = decodeHexString "1c0111001f010100061a024b53535009181c"
    s2 = decodeHexString "686974207468652062756c6c277320657965"

-- Assumption: The character that has the most chance of being repeated is
-- space (=32). The algorithm will test all keys and take the one with most space
challenge3 :: B.ByteString
challenge3 = snd $ head $ L.sortBy (\a b -> flip compare (fst a) (fst b))$ map (\x->(countCharacter 32 x, x)) bruteForceDecryptionMsgs
  where
    bruteForceDecryptionMsgs = map decryptWithKey [1..255] 
    countCharacter n bs = B.length $ B.filter (==n) bs
    decryptWithKey n = singleByteXor msg n
    msg = decodeHexString "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

challenge4 str = str
