module Main where

import MyCryptoUtil
import Set1
import qualified Data.ByteString as B
import System.IO
import Control.Exception
import Data.Maybe
import Data.Either

openFile' :: FilePath -> IO (Either IOError String)
openFile' filename = do
  results <- try $ readFile filename
  return results  

runChallenge :: (Show a) => (String -> a) -> Either IOError String -> IO ()
runChallenge challenge input = do
  case input of
    Left err -> print err
    Right contents -> putStrLn $ show $ challenge contents

runChallengeNoInput :: B.ByteString -> IO()
runChallengeNoInput = putStrLn . show

main = do
  --(openFile' "dataset/challenge4.txt") >>= (runChallenge challenge4)
  --runChallengeNoInput challenge3
  putStrLn $ show challenge5
  return ()
