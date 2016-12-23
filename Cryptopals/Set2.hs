module Set2 where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import MyCryptoUtil
--import MyAES
import Data.Word
import qualified Codec.Crypto.AES as AES
import System.Random

challenge9 = pkcs7padding msg 20
  where
    msg = C8.pack "YELLOW SUBMARINE"

-- Failed to implement it by hand.
-- Could not find any proper documentation on AES key expansion.
challenge10 str = AES.crypt' AES.CBC key initVector AES.Decrypt msg
  where
    msg = decodeBase64String $ str
    key = C8.pack "YELLOW SUBMARINE"
    initVector = B.pack $  replicate 16 (0::Word8)

padMsg msg 
  | l < 16 = pkcs7padding msg 16
  | otherwise = pkcs7padding msg (padLen + l)
  where
    l = B.length msg
    padLen = 16 - (l `mod` 16)

-- Challenge 11
-- The detection algorithm only works when there are chunks of the text that 
-- are repeated in the msg. That is, because ECB encrypts all it's blocks in the 
-- same way
isECB msg = (length $ filter (== 0) blocksHammingDistance) > 0
  where
    blocksHammingDistance = map (\(x, y) -> hammingDistance x y) $ combinations $ splitEvery 16 msg

testM = stringToBytes "YELLOW SUBMARINEYELLOW SUBMARINEYELLOW SUBMARINEYELLOW SUBMARINE"

encryptionOracle :: B.ByteString -> IO B.ByteString
encryptionOracle msg = do
    aes16Key <- randBytes 16
    initVector <- randBytes 16
    l <- (newStdGen >>= return . randomR (5::Int, 10))
    let headerLen = fst l
    headAndFoot <- randBytes headerLen
    mode <- (newStdGen >>= return . randomR (False, True))
    let fullMsg = B.concat [headAndFoot, msg, headAndFoot]
    let paddedMsg = padMsg fullMsg
    if fst mode 
      then return $ AES.crypt' AES.ECB aes16Key initVector AES.Encrypt paddedMsg
      else return $ AES.crypt' AES.CBC aes16Key initVector AES.Encrypt paddedMsg

