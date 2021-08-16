module Task03.Core
  ( part1
  , part2
  ) where

import Control.Monad.Trans.State (evalStateT, execStateT, StateT, modify, gets, get)
import Control.Monad.Loops       (iterateUntil)
import Control.Monad             (mapM)

import Data.List                 (permutations)

import Lib                       (inputFile, inputString)

import Task03.Intcode            (Intcode, initIntcode, pushInput, popOutput, runProgram)
import Task03.Intcode.Accessors  (isHalted, isOutputing)

part1 :: IO ()
part1 = do
  code <- inputFile "src/Task03/input.txt"
  let intcode = initIntcode code

  finalAmps <- sequence $ map (runAmps1 intcode . createAmps1) $ permutations [0..4]
  print $ maximum $ map output finalAmps

data Amplifier = Amp {phase, input, output :: Int} 

createAmps1 :: [Int] -> [Amplifier]
createAmps1 = map (\p -> Amp p 0 0)

runAmps1 :: Intcode -> [Amplifier] -> IO Amplifier
runAmps1 intcode (amp:[]) = runAmp1 intcode amp
runAmps1 intcode (amp1:amp2:amps) = do
  amp1' <- runAmp1 intcode amp1
  runAmps1 intcode (amp2 {input = output amp1'}:amps)

runAmp1 :: Intcode -> Amplifier -> IO Amplifier
runAmp1 intcode amp = do
  out <- evalStateT (pushInput (phase amp) >> pushInput (input amp) >> runProgram >> popOutput) intcode
  return $ amp {output = out}

part2 :: IO ()
part2 = do
  code <- inputFile "src/Task03/input.txt"
  let intcode = initIntcode code

  intcodes <- mapM (\phases -> createAmps2 intcode phases) $ permutations [5..9]
  signalList <- mapM (execStateT runAmps2 . (,) 0) intcodes
  print $ maximum $ map fst signalList

createAmps2 :: Intcode -> [Int] -> IO [Intcode]
createAmps2 intcode phases = do
  (amp1:amps) <- mapM (\p -> execStateT (pushInput p) intcode) phases
  amp1' <- execStateT (pushInput 0) amp1
  return $ amp1':amps

runAmps2 :: StateT (Int,[Intcode]) IO [Intcode]
runAmps2 = iterateUntil (isHalted . last) $ do
  intcodes <- gets snd
  intcodes' <- mapM (execStateT runProgram) intcodes
  modify $ \(signal,_) -> (signal,intcodes')
  passOutputs

passOutputs :: StateT (Int,[Intcode]) IO [Intcode]
passOutputs = do
  passOutput 0
  passOutput 1
  passOutput 2
  passOutput 3
  passOutput 4
  gets snd

passOutput :: Int -> StateT (Int,[Intcode]) IO ()
passOutput from = do
  let to = (from + 1) `mod` 5

  intcodes <- gets snd
  let icFrom = intcodes !! from
      icTo   = intcodes !! to

  if isOutputing icFrom
  then do
    (out,icFrom') <- evalStateT (popOutput >>= \o -> do ic <- get; return (o,ic)) icFrom
    icTo'   <- execStateT (pushInput out) icTo
    modify $ \(signal,intcodes) -> (if from == 4 then out else signal, insert to icTo' $ insert from icFrom' intcodes)
  else pure ()

insert :: Int -> a -> [a] -> [a]
insert index a ls = zipWith (\x idx -> if idx == index then a else x) ls [0..]


