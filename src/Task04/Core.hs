module Task04.Core
  ( part1
  , part2
  ) where

import Control.Monad.Trans.State (execStateT)
import Control.Monad             (void)

import Lib                       (inputFile)

import Task04.Intcode            (initIntcode, runProgramIO, pushInput)

part1 :: IO ()
part1 = do
  code <- inputFile "src/Task04/input.txt"
  let intcode = initIntcode code

  putStrLn "Input:  1"
  void $ execStateT (pushInput 1 >> runProgramIO) intcode

part2 :: IO ()
part2 = do
  code <- inputFile "src/Task04/input.txt"
  let intcode = initIntcode code

  putStrLn "Input:  2"
  void $ execStateT (pushInput 2 >> runProgramIO) intcode