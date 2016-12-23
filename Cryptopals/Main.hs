module Main where

import MyCryptoUtil
import Set1
import Set2
import qualified Data.ByteString as B
import System.IO
import Control.Exception
import Data.Maybe
import Data.Either
import MyAES

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
  --(openFile' "dataset/challenge10.txt") >>= (runChallenge challenge10)
  --runChallengeNoInput challenge3
--  putStrLn $ show challenge9
  return ()
