module Task04.Intcode.Accessors where

import qualified Data.IntMap               as IM

import           Control.Monad.Trans.State (gets, modify)
import           Control.Monad             (zipWithM)

import           Data.Maybe                (fromMaybe)

import           Task04.Intcode.Types      (Intcode(..), Program,
                                            State(..), Index,
                                            ParamMode(..), Param)

-- Getters/Setters
setFocus :: Monad m => Index -> Program m ()
setFocus f' = modify $ \intcode -> intcode {_focus = f'}

modFocus :: Monad m => Index -> Program m ()
modFocus f' = modify $ \intcode -> intcode {_focus = f' + _focus intcode}

codeAt :: Monad m => Index -> Program m Int
codeAt = valAt Immediate

valAt :: Monad m => ParamMode -> Index -> Program m Int
valAt mode index = do
  code <- gets _code
  let c = fromMaybe 0 $ code IM.!? index

  case mode of
    Immediate -> return c

    Position  -> return $ fromMaybe 0 $ code IM.!? c
    WritePosition -> return c

    Relative  -> do
      relBase <- gets _relBase
      return $ fromMaybe 0 $ code IM.!? (relBase + c)
    WriteRelative -> do
      relBase <- gets _relBase
      return $ relBase + c

setAt :: Monad m => Index -> Int -> Program m ()
setAt i v = do
  code <- gets _code
  modify $ \intcode -> intcode {_code = IM.insert i v code}

pushInput :: Monad m => Int -> Program m ()
pushInput input = modify $ \intcode -> intcode {_inputQueue = _inputQueue intcode ++ [input]}

popOutput :: Monad m => Program m Int
popOutput = do
  outputQueue <- gets _outputQueue
  modify $ \intcode -> intcode {_outputQueue = tail outputQueue}
  return $ head outputQueue

opcode :: Monad m => Program m Int
opcode = do
  op <- gets _focus >>= codeAt
  return $ op `mod` 100

paramModes :: Monad m => Program m [ParamMode]
paramModes = do
  op <- gets _focus >>= codeAt
  return $ parseParamModes $ op `div` 100

params :: Monad m => Int -> Program m [Param]
params argCount = do
  focus <- gets _focus
  modes <- paramModes
  zipWithM valAt modes [focus+1 .. focus+argCount]

paramsWithWrite :: Monad m => Int -> Program m [Param]
paramsWithWrite argCount = do
  focus <- gets _focus
  modes <- paramModes

  let lastMode = case modes !! (argCount-1) of
                  Position -> WritePosition
                  Relative -> WriteRelative
                  mode     -> error $ "Invalid write param mode: " ++ show mode

  let modes' = take (argCount-1) modes ++ [lastMode] ++ drop argCount modes
  zipWithM valAt modes' [focus+1 .. focus+argCount]

-- Helpers --

isRunning :: Intcode -> Bool
isRunning = (==) Running . _state

isOutputing :: Intcode -> Bool
isOutputing = (==) Outputing . _state

isHalted :: Intcode -> Bool
isHalted = (==) Halted . _state

parseParamModes :: Int -> [ParamMode]
parseParamModes 0 = repeat Position
parseParamModes n =
  (case n `mod` 10 of
    0 -> Position
    1 -> Immediate
    2 -> Relative
  ) : parseParamModes (n `div` 10)