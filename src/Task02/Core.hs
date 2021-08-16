module Task02.Core
  ( part1
  , part2
  ) where

import Control.Monad.Trans.State (evalStateT)
import Control.Monad             (void)

import Lib                       (inputFile)
import Task02.Intcode            (initIntcode, runProgramIO, pushInput)

part1 :: IO ()
part1 = do
  code <- inputFile "src/Task02/input.txt"

  let intcode = initIntcode code

  putStrLn "Input:  1"
  void $ evalStateT (pushInput 1 >> runProgramIO) intcode

part2 :: IO ()
part2 = do
  code <- inputFile "src/Task02/input.txt"

  let intcode = initIntcode code

  putStrLn "Input:  5"
  void $ evalStateT (pushInput 5 >> runProgramIO) intcode