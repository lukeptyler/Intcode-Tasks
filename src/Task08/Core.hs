module Task08.Core
  ( part1
  , part2
  ) where

import           Control.Monad.Trans.State (get, gets, modify, put)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.IO.Class    (liftIO)

import           Data.Map                  (Map)
import qualified Data.Map                  as M

import           Data.List                 (isPrefixOf, stripPrefix, sortOn)

import           Lib                       (Point, (+.), 
                                            inputFile, prompt,
                                            drawMap, 
                                            mapToList, listToMap)

import           Task08.Intcode            (Integration, IntegrationIO, 
                                            initIntcode, 
                                            runIntegration,
                                            evalIntegration, execIntegration, 
                                            evalIntegrationT,
                                            pushInputList, popOutput)

part1 :: IO ()
part1 = do
  intcode <- initIntcode <$> inputFile "src/Task08/input.txt"

  let scaffoldMap = evalIntegration getMap [""] intcode

      scaffolding = M.filter (/= '.') scaffoldMap
      interPoints = filter (\p -> and $ map (\off -> M.member (p +. off) scaffolding) [(0,1),(0,-1),(1,0),(-1,0)]) $ M.keys scaffolding

  drawMap _drawTile $ mapToList '.'  scaffoldMap

  print $ sum $ map (\(x,y) -> x*y) interPoints

part2 :: IO ()
part2 = do
  code <- inputFile "src/Task08/input.txt"
  let intcode = initIntcode $ 2 : tail code

  --findCommands

  let m = "A,B,A,C,C,A,B,C,B,B"
      a = "L,8,R,10,L,8,R,8"
      b = "L,12,R,8,R,8"
      c = "L,8,R,6,R,6,R,10,L,8"

  evalIntegrationT runBot ([m,a,b,c], True) intcode

getMap :: Integration [String] (Map Point Char)
getMap = do
  runIntegration handleInput handleOutput
  listToMap <$> get
  where
    handleInput :: Integration [String] ()
    handleInput = pure ()

    handleOutput :: Integration [String] ()
    handleOutput = do
      output <- lift popOutput
      case output of
        10 -> modify $ \lss -> lss ++ [""]
        n  -> modify $ \lss -> init lss ++ [last lss ++ [toEnum n]]

runBot :: IntegrationIO ([String],Bool) ()
runBot = runIntegration handleInput handleOutput
  where
    handleInput :: IntegrationIO ([String],Bool) ()
    handleInput = do
      inputList <- gets fst

      if null inputList
      then do
        input <- liftIO getLine
        lift $ pushInputList $ map fromEnum $ input ++ "\n"
        put ([],True)
      else do
        liftIO $ putStrLn $ head inputList
        lift $ pushInputList $ map fromEnum $ (head inputList) ++ "\n"
        put (tail inputList,True)

    handleOutput :: IntegrationIO ([String],Bool) ()
    handleOutput = do
      output <- lift popOutput

      handleChar output
      --handleChar output

    handleChar :: Int -> IntegrationIO ([String],Bool) ()
    handleChar output
      | output > 127 = liftIO $ print output
      | otherwise    = do
        let c = toEnum output

        isMap <- if c `notElem` "#.X^v><\n"
                 then pure False
                 else gets snd

        if isMap
        then liftIO $ putStr $ _drawTile c
        else liftIO $ putChar c

        modify $ \(inputs,_) -> (inputs, isMap)

_drawTile :: Char -> String
_drawTile '#' = "\x1b[43m \x1b[0m"
_drawTile '.' = "\x1b[40m \x1b[0m"
_drawTile 'X' = "\x1b[40m\x1b[37m\x1b[1mX\x1b[0m"
_drawTile '^' = "\x1b[43m\x1b[1m^\x1b[0m"
_drawTile '>' = "\x1b[43m\x1b[1m>\x1b[0m"
_drawTile '<' = "\x1b[43m\x1b[1m<\x1b[0m"
_drawTile 'v' = "\x1b[43m\x1b[1mv\x1b[0m"
_drawTile c   = [c]


-- Solve part 2 --
data Move = L | R | G | Go Int
  deriving (Eq)
instance Show Move where
  show L = "L"
  show R = "R"
  show G = "G"
  show (Go n) = show n

type Chunk = [Move]

data Command = A | B | C
  deriving (Show)

findCommands :: IO ()
findCommands = do
  let path = [L] ++ (replicate 8 G) ++ [R] ++ (replicate 10 G) ++ [L] ++ (replicate 8 G) ++ [R] ++ (replicate 8 G) ++ [L] ++ (replicate 12 G) ++ [R] ++ (replicate 8 G) ++ [R] ++ (replicate 8 G) ++ [L] ++ (replicate 8 G) ++ [R] ++ (replicate 10 G) ++ [L] ++ (replicate 8 G) ++ [R] ++ (replicate 8 G) ++ [L] ++ (replicate 8 G) ++ [R] ++ (replicate 6 G) ++ [R] ++ (replicate 6 G) ++ [R] ++ (replicate 10 G) ++ [L] ++ (replicate 8 G) ++ [L] ++ (replicate 8 G) ++ [R] ++ (replicate 6 G) ++ [R] ++ (replicate 6 G) ++ [R] ++ (replicate 10 G) ++ [L] ++ (replicate 8 G) ++ [L] ++ (replicate 8 G) ++ [R] ++ (replicate 10 G) ++ [L] ++ (replicate 8 G) ++ [R] ++ (replicate 8 G) ++ [L] ++ (replicate 12 G) ++ [R] ++ (replicate 8 G) ++ [R] ++ (replicate 8 G) ++ [L] ++ (replicate 8 G) ++ [R] ++ (replicate 6 G) ++ [R] ++ (replicate 6 G) ++ [R] ++ (replicate 10 G) ++ [L] ++ (replicate 8 G) ++ [L] ++ (replicate 12 G) ++ [R] ++ (replicate 8 G) ++ [R] ++ (replicate 8 G) ++ [L] ++ (replicate 12 G) ++ [R] ++ (replicate 8 G) ++ [R] ++ (replicate 8 G)
  
  let allChunkSizes = [(aSize,bSize,cSize) | aSize <- [1..length path - 2], bSize <- [1..length path - 1 - aSize], cSize <- [1..length path - aSize - bSize]]

  testAllChunks path allChunkSizes

removeAllPrefixes :: [Chunk] -> [Move] -> [Move]
removeAllPrefixes chunks path
  | null prefixChunks = path
  | otherwise        = removeAllPrefixes chunks $ drop (length chunk) path
  where
    prefixChunks = filter (`isPrefixOf` path) $ reverse $ sortOn length chunks
    chunk = head prefixChunks

isSizeOk :: Show a => [a] -> Bool
isSizeOk ls = length (show ls) - 2 <= 20

condense :: Chunk -> Chunk
condense [] = []
condense (L:ls) = L : condense ls
condense (R:ls) = R : condense ls
condense (G:ls) = (Go $ length gs + 1) : condense ls'
  where
    (gs,ls') = span (== G) ls

convert :: (Chunk,Chunk,Chunk) -> [Chunk] -> [Command]
convert _       [] = []
convert pieces@(a,b,c) (chunk:chunks)
  | a == chunk = A : rest
  | b == chunk = B : rest
  | c == chunk = C : rest
  where
    rest = convert pieces chunks

testAllChunks :: [Move] -> [(Int,Int,Int)] -> IO ()
testAllChunks _    []                                 = putStrLn "No valid chunk sizes"
testAllChunks path (sizes@(aSize,bSize,cSize):chunks) = do
  let a = take aSize path
      b = take bSize $ removeAllPrefixes [a] path
      c = take cSize $ removeAllPrefixes [a,b] path

  if or $ map (not . isSizeOk . condense) [a,b,c]
  then fail
  else do
    let commands = filter isSizeOk $ map (convert (a,b,c)) $ createCommands path (a,b,c)

    if null commands
    then fail
    else do
      mapM_ (putStrLn . ("Main: " ++) . show) commands
      putStrLn $ "A: " ++ show (condense a)
      putStrLn $ "B: " ++ show (condense b)
      putStrLn $ "C: " ++ show (condense c)
  where
    fail
      | bSize == 1 && cSize == 1= do
        putStrLn $ "Not " ++ show sizes
        testAllChunks path chunks
      | otherwise = testAllChunks path chunks


createCommands :: [Move] -> (Chunk,Chunk,Chunk) -> [[Chunk]]
createCommands []   _                = [[]]
createCommands path commands@(a,b,c) = afterA ++ afterB ++ afterC
  where
    afterA :: [[Chunk]]
    afterA
      | a `isPrefixOf` path = map (a :) $ createCommands (drop (length a) path) commands
      | otherwise           = []

    afterB :: [[Chunk]]
    afterB
      | b `isPrefixOf` path = map (b :) $ createCommands (drop (length b) path) commands
      | otherwise           = []

    afterC :: [[Chunk]]
    afterC
      | c `isPrefixOf` path = map (c :) $ createCommands (drop (length c) path) commands
      | otherwise           = []