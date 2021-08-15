module Main where

import           Control.Monad.Loops (untilM_)
import           System.IO           (hFlush, stdout)
import           Text.Read           (readEither)

import           Data.IntMap         (IntMap)
import qualified Data.IntMap         as IM

import qualified Task01.Core         as Core1

main :: IO ()
main = do
  taskHeader
  untilM_ taskPrompt exitPrompt
  putStrLn "Closing Intcode Tasks"

taskHeader :: IO ()
taskHeader = do
  putStrLn "\nTask List:"
  mapM_ (putStrLn . (++) "  ") $ map name taskList
  putStrLn ""

taskPrompt :: IO ()
taskPrompt = do
  selection <- prompt "Enter the task number to run (? for task list): "
  case readEither selection of
    Left  _       -> (if selection == "?"
                      then taskHeader
                      else putStrLn "  Invalid Input") >> taskPrompt
    Right taskNum -> if taskNum `IM.member` taskMap
                     then runTask $ taskMap IM.! taskNum
                     else putStrLn "  Invalid Task Number" >> taskPrompt

exitPrompt :: IO Bool
exitPrompt = do
  again <- prompt "Do you want to run another task (y/n): "
  case again of
    "y" -> return False
    "n" -> return True
    _   -> putStrLn "  Please enter 'y' or 'n'" >> exitPrompt

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

data Task = Task {name :: String, part1, part2 :: IO ()}

runTask :: Task -> IO ()
runTask task = do
  putStrLn "\n---- PART 1 ----"
  part1 task
  putStrLn "\n---- PART 2 ----"
  part2 task
  putStrLn ""

taskList :: [Task]
taskList =
  [ Task "Task (1): 1202 Program Alarm" Core1.part1 Core1.part2
  ]

taskMap :: IntMap Task
taskMap = IM.fromAscList $ zip [1..] taskList