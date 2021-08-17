module Task05.Core
  ( part1
  , part2
  ) where

import           Control.Monad.Trans.State (get, gets, modify)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad             (void)

import           Data.Map                  (Map)
import qualified Data.Map                  as M

import           Data.Maybe                (fromMaybe)

import           Lib                       (inputFile)
import           Task05.Intcode            (Intcode, Integration,
                                            initIntcode,
                                            execIntegration, runIntegration,
                                            pushInput, popOutput)

part1 :: IO ()
part1 = do
  intcode <- initIntcode <$> inputFile "src/Task05/input.txt"

  let bot = Bot (0,0) N Paint
      hull = M.empty

  let (bot',hull') = execIntegration runBot (bot, hull) intcode
  print $ M.size hull'

part2 :: IO ()
part2 = do
  intcode <- initIntcode <$> inputFile "src/Task05/input.txt"

  let bot = Bot (0,0) N Paint
      hull = M.singleton (0,0) 1

  let (bot',hull') = execIntegration runBot (bot, hull) intcode
  _draw hull' bot'

type Hull = Map (Int,Int) Int

data Dir = N | E | S | W
data NextCommand = Paint | Turn
data Bot = Bot {pos :: (Int,Int), dir :: Dir, nextCommand :: NextCommand}

getColor :: Bot -> Hull -> Int
getColor bot hull = fromMaybe 0 $ hull M.!? (pos bot)

setColor :: Bot -> Hull -> Int -> Hull
setColor bot hull color = M.insert (pos bot) color hull

move :: Bot -> Bot
move bot@Bot{pos = (x,y), dir = N} = bot {pos = (x,y+1)}
move bot@Bot{pos = (x,y), dir = E} = bot {pos = (x+1,y)}
move bot@Bot{pos = (x,y), dir = S} = bot {pos = (x,y-1)}
move bot@Bot{pos = (x,y), dir = W} = bot {pos = (x-1,y)}

turnLeft :: Bot -> Bot
turnLeft bot@Bot{dir = N} = bot {dir = W}
turnLeft bot@Bot{dir = E} = bot {dir = N}
turnLeft bot@Bot{dir = S} = bot {dir = E}
turnLeft bot@Bot{dir = W} = bot {dir = S}

turnRight :: Bot -> Bot
turnRight bot@Bot{dir = N} = bot {dir = E}
turnRight bot@Bot{dir = E} = bot {dir = S}
turnRight bot@Bot{dir = S} = bot {dir = W}
turnRight bot@Bot{dir = W} = bot {dir = N}

runBot :: Integration (Bot,Hull) ()
runBot = void $ runIntegration handleInput handleOutput
  where
    handleInput :: Integration (Bot,Hull) ()
    handleInput = do
      (bot,hull) <- get
      lift $ pushInput $ getColor bot hull

    handleOutput :: Integration (Bot,Hull) ()
    handleOutput = do
      commandType <- gets (nextCommand . fst)
      command <- lift popOutput

      (bot,hull) <- get
      case commandType of
        Paint -> do
          let bot' = bot {nextCommand = Turn}
              hull' = setColor bot hull command

          modify $ const (bot', hull')

        Turn  -> do
          let bot' = (move $ (if command == 0 then turnLeft else turnRight) bot) {nextCommand = Paint}
          modify $ const (bot', hull)

_draw :: Hull -> Bot -> IO ()
_draw hull bot = sequence_ $ map drawRow $ map (\y -> map (\x -> (x,y)) [leftBound .. rightBound]) [topBound,topBound-1 .. bottomBound]
  where
    keys = M.keys hull
    leftBound
      | null keys = -2
      | otherwise = min (-2) $ minimum $ map fst keys
    rightBound
      | null keys = 2
      | otherwise = max 2 $ maximum $ map fst keys
    bottomBound
      | null keys = -2
      | otherwise = min (-2) $ minimum $ map snd keys
    topBound
      | null keys = 2
      | otherwise = max 2 $ maximum $ map snd keys

    drawRow row = putStrLn $ map drawCell row

    drawCell cell
      | cell == pos bot = case dir bot of
                              N -> '^'
                              E -> '>'
                              S -> 'v'
                              W -> '<'
      | otherwise = if (fromMaybe 0 $ hull M.!? cell) == 0 then '.' else '#'