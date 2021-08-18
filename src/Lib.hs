module Lib where

import System.IO            (hFlush, stdout)
import Data.List.Split      (splitOn)

import           Data.Map   (Map)
import qualified Data.Map   as M

import           Data.Set   (Set)
import qualified Data.Set   as S

import           Data.Maybe (fromMaybe)

type Point = (Int,Int)
(+.) :: Point -> Point -> Point
(x1,y1) +. (x2,y2) = (x1+x2,y1+y2)

inputFile :: FilePath -> IO [Int]
inputFile = (inputString <$>) . readFile

inputString :: String -> [Int]
inputString = map read . splitOn ","

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

clearTerminal :: IO ()
clearTerminal = putStr $ (toEnum 27) : "[H" ++ [toEnum 27] ++ "[J"

mapToList :: a -> Map Point a -> [[(Point,a)]]
mapToList defaultTile m = map (\y -> map (\x -> ((x,y), fromMaybe defaultTile $ m M.!? (x,y))) [leftBound .. rightBound]) [topBound .. bottomBound]
  where
    keys = M.keys m
    leftBound   = minimum $ map fst keys
    rightBound  = maximum $ map fst keys
    topBound    = minimum $ map snd keys
    bottomBound = maximum $ map snd keys

mapToListBorder :: Int -> a -> Map Point a -> [[(Point,a)]]
mapToListBorder border defaultTile m = map (\y -> map (\x -> ((x,y), fromMaybe defaultTile $ m M.!? (x,y))) [leftBound .. rightBound]) [topBound .. bottomBound]
  where
    keys = M.keys m
    leftBound   = minimum (map fst keys) - border
    rightBound  = maximum (map fst keys) + border
    topBound    = minimum (map snd keys) - border
    bottomBound = maximum (map snd keys) + border

floodFillAdj :: Point -> [Point] -> Map Point Int
floodFillAdj start points = let initialPoints = S.delete start $ S.fromList points
                            in  go (M.singleton start 0) $ neighbors initialPoints (start,0)
  where
    neighbors :: Set Point -> (Point,Int) -> (Set Point, [(Point,Int)])
    neighbors unvisited (point,dist) = (unvisited S.\\ neighborSet, zip (S.toList neighborSet) $ repeat (dist+1))
      where
        neighborSet = S.intersection unvisited $ S.fromList $ map (point +.) [(0,-1),(1,0),(0,1),(-1,0)]

    go :: Map Point Int -> (Set Point, [(Point,Int)]) -> Map Point Int
    go m (_, [])      = m
    go m (s, q:queue) = go (uncurry M.insert q m) (s', queue ++ newNeighbors)
      where
        (s',newNeighbors) = neighbors s q

drawMap :: ((Point,a) -> String) -> [[(Point,a)]] -> IO ()
drawMap drawTile = mapM_ (putStrLn . concatMap drawTile)

drawFloodMap :: Map Point Int -> IO ()
drawFloodMap m = drawMap _drawTile $ mapToListBorder 1 (-1) m
  where
    maxFlood = maximum $ M.elems m

    _drawTile :: (Point, Int) -> String
    _drawTile (_, -1)    = "\x1b[40m \x1b[0m"
    _drawTile (_, flood) = "\x1b[48;5;" ++ show color ++ "m \x1b[0m"
      where
        color = 40 + round (5 * fromIntegral (maxFlood - flood) / (fromIntegral maxFlood + 1))




