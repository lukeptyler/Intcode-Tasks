module Task10.Core
  ( part1
  , part2
  ) where

import Control.Monad.Trans.State (get, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class    (liftIO)

import Lib (inputFile)
import Task10.Intcode (IntegrationIO, 
                       initIntcode, 
                       runIntegrationASCII, evalIntegrationT,
                       popOutput, pushInputList
                       , runProgramASCII, evalProgramT)

part1 :: IO ()
part1 = do
  intcode <- initIntcode <$> inputFile "src/Task10/input.txt"

  putStrLn "¬(A ∧ B ∧ C) ∧ D\n"
  let program = [ "OR A T"
                , "AND B T"
                , "AND C T"
                , "NOT T J"
                , "AND D J"
                , "WALK"
                ]

  evalIntegrationT runSpringdroid program intcode

part2 :: IO ()
part2 = do
  intcode <- initIntcode <$> inputFile "src/Task10/input.txt"

  putStrLn "¬(A ∧ B ∧ C) ∧ D ∧ (E ∨ H)\n"
  let program = [ "OR A T"
                , "AND B T"
                , "AND C T"
                , "NOT T J"
                , "AND D J"
                , "OR E T"
                , "OR H T"
                , "AND T J"
                , "RUN"
                ]

  evalIntegrationT runSpringdroid program intcode

runSpringdroid :: IntegrationIO [String] ()
runSpringdroid = runIntegrationASCII handleInput (const $ pure ())
  where
    handleInput :: IntegrationIO [String] ()
    handleInput = do
      (input:inputQueue) <- get
      lift $ pushInputList $ map fromEnum $ input ++ "\n"
      liftIO $ putStrLn $ "Input: " ++ input
      put inputQueue