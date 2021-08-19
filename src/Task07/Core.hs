module Task07.Core
  ( part1
  , part2
  ) where

import           Control.Monad.Trans.State (modify, get, gets, put)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.IO.Class    (liftIO)

import           Data.Map                  (Map)
import qualified Data.Map                  as M

import           Data.List                 (find)
import           Data.Maybe                (fromMaybe, maybe)

import           Lib                       (inputFile,
                                            Point, (+.), 
                                            clearTerminal, 
                                            mapToListPoints, floodFillAdj,
                                            drawMapPoints, drawFloodMap)

import           Task07.Intcode            (IntegrationIO, Integration, 
                                            initIntcode, 
                                            evalIntegrationT, evalIntegration,
                                            runIntegration, 
                                            pushInput, popOutput)
import           Task07.Intcode.Types      (Intcode(_state), State(Halted))

part1 :: IO ()
part1 = do
  intcode <- initIntcode <$> inputFile "src/Task07/input.txt"

  let defaultMap   = M.singleton (0,0) Empty
      defaultDroid = Droid (0,0) NoDir

  let maze = evalIntegration explore (defaultMap, defaultDroid, []) intcode
      oxygenSystemPoint = fst $ M.elemAt 0 $ M.filter (== OxygenSystem) maze
      floodMap = floodFillAdj (0,0) $ M.keys $ M.filter (/= Wall) maze
  
  drawFloodMap floodMap
  print $ floodMap M.! oxygenSystemPoint

part2 :: IO ()
part2 = do
  intcode <- initIntcode <$> inputFile "src/Task07/input.txt"

  let defaultMap   = M.singleton (0,0) Empty
      defaultDroid = Droid (0,0) NoDir

  let maze = evalIntegration explore (defaultMap, defaultDroid, []) intcode
      oxygenSystemPoint = fst $ M.elemAt 0 $ M.filter (== OxygenSystem) maze
      floodMap = floodFillAdj oxygenSystemPoint $ M.keys $ M.filter (/= Wall) maze

  drawFloodMap floodMap
  print $ maximum $ M.elems floodMap

data Tile = Unvisited
          | Empty
          | Wall
          | OxygenSystem
  deriving (Eq)

data Dir = NoDir | N | S | W | E
  deriving (Enum)

dirToPoint :: Dir -> Point
dirToPoint NoDir = (0,0)
dirToPoint N     = (0,-1)
dirToPoint S     = (0,1)
dirToPoint W     = (-1,0)
dirToPoint E     = (1,0)

oppositeDir :: Dir -> Dir
oppositeDir N = S
oppositeDir S = N
oppositeDir W = E
oppositeDir E = W

data Droid = Droid {_pos :: Point, _dir :: Dir}

explore :: Integration (Map Point Tile, Droid, [Dir]) (Map Point Tile)
explore = do
  runIntegration handleInput handleOutput

  (maze,_,_) <- get
  return maze

  where
    handleInput :: Integration (Map Point Tile, Droid, [Dir]) ()
    handleInput = do
      (maze,droid,stack) <- get
      let mNext = find (flip M.notMember maze . fst) $ map (\dir -> (_pos droid +. dirToPoint dir, dir)) [N,E,S,W]

      case mNext of
        Nothing      -> 
          if null stack
          then lift $ modify $ \intcode -> intcode {_state = Halted}
          else do
            let goBack = head stack
            lift $ pushInput $ fromEnum goBack
            modify $ \(m,d,s) -> (m, d{_dir = goBack}, tail s)
        Just (_,dir) -> do
          lift $ pushInput $ fromEnum dir
          modify $ \(m,d,s) -> (m, d{_dir = dir}, oppositeDir dir : s)

    handleOutput :: Integration (Map Point Tile, Droid, [Dir]) ()
    handleOutput = do
      output <- lift popOutput

      (maze,droid,stack) <- get
      let point = dirToPoint (_dir droid) +. _pos droid
      case output of
        0 -> put (M.insert point Wall         maze, droid {_dir = NoDir}, tail stack)
        1 -> put (M.insert point Empty        maze, Droid point NoDir,    stack)
        2 -> put (M.insert point OxygenSystem maze, Droid point NoDir,    stack)

exploreInteractive :: IntegrationIO (Map Point Tile, Droid) ()
exploreInteractive = runIntegration handleInput handleOutput
  where
    handleInput :: IntegrationIO (Map Point Tile, Droid) ()
    handleInput = do
      input <- liftIO $ getChar
      liftIO $ putStr "\b "

      case input of
        'w' -> setDir N >> lift (pushInput 1)
        's' -> setDir S >> lift (pushInput 2)
        'a' -> setDir W >> lift (pushInput 3)
        'd' -> setDir E >> lift (pushInput 4)
        _ -> pure ()

    handleOutput :: IntegrationIO (Map Point Tile, Droid) ()
    handleOutput = do
      output <- lift popOutput

      (m, droid) <- get
      let point = dirToPoint (_dir droid) +. _pos droid
          (m', droid') = case output of
            0 -> (M.insert point Wall         m, droid {_dir = NoDir})
            1 -> (M.insert point Empty        m, Droid point NoDir)
            2 -> (M.insert point OxygenSystem m, Droid point NoDir)

      put (m', droid')

      liftIO clearTerminal
      liftIO $ _drawMap m' droid'

    setDir :: Dir -> IntegrationIO (Map Point Tile, Droid) ()
    setDir dir = modify $ \(m, droid) -> (m, droid {_dir = dir})

_drawMap :: Map Point Tile -> Droid -> IO ()
_drawMap m droid = drawMapPoints _drawTile $ mapToListPoints Unvisited m
  where
    _drawTile :: (Point, Tile) -> String
    _drawTile (_    , Unvisited)    = "\x1b[48;5;145m \x1b[0m"
    _drawTile (point, Empty)        = _drawDroid point
    _drawTile (_    , Wall)         = "\x1b[40m \x1b[0m"
    _drawTile (point, OxygenSystem) = "\x1b[44m\x1b[38;5;231m" ++ _drawDroid point ++ "\x1b[0m"

    _drawDroid :: Point -> String
    _drawDroid point 
      | point == _pos droid = "\x1b[1mD\x1b[0m"
      | otherwise           = " "



