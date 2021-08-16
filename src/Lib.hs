module Lib 
  ( inputFile
  , inputString
  , prompt
  ) where

import System.IO       (hFlush, stdout)
import Data.List.Split (splitOn)

inputFile :: FilePath -> IO [Int]
inputFile = (inputString <$>) . readFile

inputString :: String -> [Int]
inputString = map read . splitOn ","

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine