module Set1 where

import MyCryptoUtil
import qualified Data.ByteString as B
import Data.Bits

challenge1 :: B.ByteString
challenge1 = encodeBytesToBase64 $ decodeHexString hexStr
  where
    hexStr = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

challenge2 :: B.ByteString
challenge2 = encodeStringToHex $ zipBytesWith xor s1 s2
  where
    s1 = decodeHexString "1c0111001f010100061a024b53535009181c"
    s2 = decodeHexString "686974207468652062756c6c277320657965"
