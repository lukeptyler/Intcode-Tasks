module Task01.Core
  ( part1
  , part2
  ) where

import Control.Monad.Trans.State (evalStateT)
import Data.Monoid               (First(..))
import Data.Bool                 (bool)

import Lib                       (inputFile)

import Task01.Intcode            (initIntcode, runUntilHalt, codeAt)

part1 :: IO ()
part1 = do
  input  <- inputFile "src/Task01/input.txt"
  let intcode = initIntcode $ modifyInput 12 2 input

  answer <- evalStateT (runUntilHalt >> codeAt 0) intcode
  print answer

part2 :: IO ()
part2 = do
  input  <- inputFile "src/Task01/input.txt"
  let intcode = initIntcode $ modifyInput 52 96 input

  answer <- evalStateT (runUntilHalt >> codeAt 0) intcode
  if answer == 19690720
  then print $ 100 * 52 + 96
  else putStrLn "???"

-- (52 96)
part2_Calculation :: IO ()
part2_Calculation = do
  input  <- inputFile "src/Task01/input.txt"
  let pairs = [(noun,verb) | noun <- [0..99], verb <- [0..99]]

  answers <- sequence $ map (\(n, v) -> bool (First Nothing) (First $ Just $ 100 * n + v) <$> validPair n v input) pairs
  print $ (\(Just answer) -> answer) $ getFirst $ mconcat answers

validPair :: Int -> Int -> [Int] -> IO Bool
validPair noun verb = ((==) 19690720 <$>) . evalStateT (runUntilHalt >> codeAt 0) . initIntcode . modifyInput noun verb

modifyInput :: Int -> Int -> [Int] -> [Int]
modifyInput x y ls = head ls : [x,y] ++ drop 3 ls