module Main where

import           Control.Monad.Loops (untilM_)
import           Text.Read           (readEither)

import           Data.IntMap         (IntMap)
import qualified Data.IntMap         as IM

import           Lib                 (prompt)

import qualified Task01.Core         as Core1
import qualified Task02.Core         as Core2
import qualified Task03.Core         as Core3
import qualified Task04.Core         as Core4
import qualified Task05.Core         as Core5
import qualified Task06.Core         as Core6
import qualified Task07.Core         as Core7
import qualified Task08.Core         as Core8

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

data Task = Task {name :: String, part1, part2 :: IO ()}

runTask :: Task -> IO ()
runTask task = do
  putStrLn ""
  putStrLn $ name task
  putStrLn "\n---- PART 1 ----"
  part1 task
  putStrLn "\n---- PART 2 ----"
  part2 task
  putStrLn ""

taskList :: [Task]
taskList =
  [ Task "Task (1): 1202 Program Alarm"               Core1.part1 Core1.part2
  , Task "Task (2): Sunny with a Chance of Asteroids" Core2.part1 Core2.part2
  , Task "Task (3): Amplification Circuit"            Core3.part1 Core3.part2
  , Task "Task (4): Sensor Boost"                     Core4.part1 Core4.part2
  , Task "Task (5): Space Police"                     Core5.part1 Core5.part2
  , Task "Task (6): Care Package"                     Core6.part1 Core6.part2
  , Task "Task (7): Oxygen System"                    Core7.part1 Core7.part2
  , Task "Task (8): Set and Forget"                   Core8.part1 Core8.part2
  ]

taskMap :: IntMap Task
taskMap = IM.fromAscList $ zip [1..] taskList