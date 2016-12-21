module Set1 where

import MyCryptoUtil
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
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
challenge3 = answer
  where
    answer = B.concat [(C8.pack ("The Encryption key is: " ++ show (fst singleByteXorMsg) ++ " and the message is: ")), snd singleByteXorMsg]
    singleByteXorMsg = findSingleByteXOR msg
    msg = decodeHexString "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

-- Receives a string a returns the one that has the most number of spaces
--findSingleByteXOR :: B.ByteString -> (Int, B.ByteString)
findSingleByteXOR msg = takeStringWithMostSpaces
  where
    takeStringWithMostSpaces = snd $ head $ L.sortBy (\a b -> flip compare (fst a) (fst b))$ map (\x->(countCharacter 32 (snd x), x)) bruteForceDecrypt
    countCharacter n bs = B.length $ B.filter (==n) bs
    bruteForceDecrypt = map (\x-> (x, singleByteXor msg x)) [1..255]


findSingleByteXORPossibilities msg = takeStringWithMostSpaces
  where
    takeStringWithMostSpaces = map snd $ take 2 $ L.sortBy (\a b -> flip compare (fst a) (fst b))$ map (\x->(countCharacter 32 (snd x), x)) bruteForceDecrypt
    countCharacter n bs = B.length $ B.filter (==n) bs
    bruteForceDecrypt = map (\x-> (x, singleByteXor msg x)) [1..255]


challenge4 fileStrings = mostLikelyXorMsg 
  where
    mostLikelyXorMsg =  map snd $ take 3 $ L.sortBy (\a b -> flip compare (fst a) (fst b))$ map (\x->(countCharacter 32 (snd x), x)) xoredMsgs
    countCharacter n bs = B.length $ B.filter (==n) bs
    xoredMsgs = concatMap (findSingleByteXORPossibilities . decodeHexString) msgs
    msgs = lines fileStrings

challenge5 = encodeStringToHex $ repeatedXOR msg key
  where 
    msg = stringToBytes "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
    key = stringToBytes "ICE"


-- We split the cipher in blocks of n key size. Then, we take the first 6 blocks and we compute the hamming distance of all combinations.
-- Technically, the key should have the smallest normalized hamming distance
--keySize :: (Ord a, Fractional a) => B.ByteString -> [(a, Int)]
keySize cipher = snd . head $ L.sortBy (\x y -> compare (fst x) (fst y)) $ map hammingDistanceForKeySize [2..40]
  where
    hammingDistanceForKeySize n = ((realToFrac $ sum $ map (\(x, y) -> hammingDistance x y) (sampleCombinationsForKeySize n)) / (realToFrac n), n)
    sampleCombinationsForKeySize n = combinations $ take 6 $ splitEvery n cipher

characterHistogram xs = map charFrequency [1..255]
  where
    charFrequency n = (n, 100.0 * (realToFrac $ length $ filter (==n) chars)/(realToFrac l))
    chars = B.unpack xs 
    l = B.length xs

-- find the percentage of letter from the histogram (Char, percentage)
percentageOfLettersFromHistogram hist = sum $ map (\(n, p) -> if isCaeserChar n then p else 0.0) hist
  where 
    isCaeserChar n = (n >= 63 && n <= 90) || (n >= 97 && n <= 122) || n == 32 || n == 33

checkAllKeysForCipher xs = take 3 $ L.sortBy (\x y -> flip compare (snd x) (snd y)) decodedCiphers 
  where
    decodedCiphers = map (applyKeyAndCheckHistogram xs) [1..255]

applyKeyAndCheckHistogram cipher key = (key, percentageOfLetters) 
  where
    decodedCipher = singleByteXor cipher key
    hist = characterHistogram decodedCipher
    percentageOfLetters = percentageOfLettersFromHistogram hist
    
-- We need to find the key Size
-- Then, transpose the bytestring so that all the bytes that would be xored with the same character are in the same array.
challenge6 str = answer
  where
    answer = B.concat [stringToBytes "Key value: ", keyTest, stringToBytes "Message Value: ", repeatedXOR msg keyTest]
    keyTest = B.pack $ map (fst . head) potentialKeys
    potentialKeys = map checkAllKeysForCipher cipherColumns
    cipherColumns = B.transpose $ splitEvery kSize msg
    msg = decodeBase64String str
    kSize = keySize msg
