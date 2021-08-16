module Lib 
  ( inputFile
  , inputString
  ) where

import Data.List.Split (splitOn)

inputFile :: FilePath -> IO [Int]
inputFile = (inputString <$>) . readFile

inputString :: String -> [Int]
inputString = map read . splitOn ","