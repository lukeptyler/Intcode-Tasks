module Task09.Core
  ( part1
  , part2
  ) where

import Control.Monad.Trans.State (get, gets, modify, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class    ()

import Lib            (inputFile, Point, (+.))
import Task09.Intcode (Intcode, Integration,
                       initIntcode, 
                       execIntegration, runIntegration, 
                       pushInputList, popOutput)

part1 :: IO ()
part1 = do
  intcode <- initIntcode <$> inputFile "src/Task09/input.txt"

  let m = map (\y -> map (\x -> if inTractorBeam intcode (x,y) then '#' else '.') [0..49]) [0..49]

  print $ length $ filter (== '#') $ concat m

part2 :: IO ()
part2 = do
  intcode <- initIntcode <$> inputFile "src/Task09/input.txt"

  print $ (\(x,y) -> 10000 * x + y) $ closestSquare 100 intcode
  
outline :: Int -> Intcode -> IO ()
outline size intcode = sequence_ $ zipWith (\(xStart,_) (xEnd,_) -> putStrLn $ mkStr xStart xEnd) (take size $ bottomLine intcode) (take size $ topLine intcode)
  where
    mkStr :: Int -> Int -> String
    mkStr start end
      | start == end = replicate start '.' ++ "#"
      | otherwise    = replicate start '.' ++ "#" ++ replicate (end - start - 1) '.' ++ "#"

closestSquare :: Int -> Intcode -> Point
closestSquare size intcode = ((+. (0,-step)) . fst) $ head $ dropWhile (\(bottom, top) -> (bottom +. (0,-step)) /= (top +. (-step,0))) $ zip (drop step $ bottomLine intcode) (topLine intcode)
  where
    step = size - 1

inTractorBeam :: Intcode -> Point -> Bool
inTractorBeam intcode (x,y) = execIntegration (lift (pushInputList [x,y]) >> runIntegration (pure ()) handleOutput) False intcode
  where
    handleOutput :: Integration Bool ()
    handleOutput = do
      output <- lift popOutput
      put $ output == 1

bottomLine :: Intcode -> [Point]
bottomLine intcode = nextPoint start
  where
    start = (3,5)

    nextPoint :: Point -> [Point]
    nextPoint p
      | inTractorBeam intcode p = p : nextPoint (p +. (0,1))
      | otherwise               = nextPoint $ p +. (1,0)

topLine :: Intcode -> [Point]
topLine intcode = nextPoint start
  where
    start = (3,5)

    nextPoint :: Point -> [Point]
    nextPoint p
      | not $ inTractorBeam intcode p = (p +. (-1,0)) : nextPoint (p +. (0,1))
      | otherwise                     = nextPoint $ p +. (1,0)