{-# LANGUAGE RankNTypes #-}
module Task02.Intcode 
  ( Intcode
  , Program
  , ProgramIO
  , initIntcode
  , runProgram
  , runProgramIO
  , pushInput
  , popOutput
  , step
  )where

import           Data.Array                (Array)
import qualified Data.Array                as A

import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.State (StateT, gets, get, modify)
import           Control.Monad.Loops       (iterateUntil)
import           Control.Monad             (zipWithM)

import           Data.Bool                 (bool)

import           Lib                       (prompt)

-- Intcode --

type Index   = Int
type Param   = Int
type Code    = Array Index Int
data State   = Running
             | Halted
             | RequestInput
             | Outputing
  deriving (Show, Eq)

data Intcode = Intcode 
  { _state :: State
  , _focus :: Index
  , _inputQueue, _outputQueue :: [Int]
  , _callback :: forall m. Monad m => Maybe (Program m ())
  , _code :: Code}
instance Show Intcode where
  show intcode = 
    "Intcode {" ++ show (_state intcode) ++ 
    " | " ++ show (_focus intcode) ++ 
    " | " ++ show (A.elems $ _code intcode) ++ "}" ++
    (if null $ _inputQueue  intcode then "" else "\ninputQueue = "  ++ show (_inputQueue  intcode)) ++
    (if null $ _outputQueue intcode then "" else "\noutputQueue = " ++ show (_outputQueue intcode))

initIntcode :: [Int] -> Intcode
initIntcode codeData = Intcode 
  { _state = Running
  , _focus = 0
  , _inputQueue = []
  , _outputQueue = []
  , _callback = Nothing
  , _code = A.listArray (0,length codeData-1) codeData
  }

-- Program --

type Program   = StateT Intcode
type ProgramIO = Program IO

data ParamMode = Position
               | Immediate
  deriving (Show, Eq)

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
  let c = code A.! index

  case mode of
    Position  -> return $ code A.! c
    Immediate -> return c

setAt :: Monad m => Index -> Int -> Program m ()
setAt i v = do
  code <- gets _code
  modify $ \intcode -> intcode {_code = code A.// [(i, v)]}

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
  let modes' = take (argCount-1) modes ++ [Immediate] ++ drop argCount modes
  zipWithM valAt modes' [focus+1 .. focus+argCount]

-- Run Program --
step_ :: Monad m => Program m ()
step_ = do
  state <- gets _state
  stepState_ state
    
stepState_ :: Monad m => State -> Program m ()
stepState_ Running = do
  op <- opcode
  case op of
    1  -> paramsWithWrite 3 >>= opAdd
    2  -> paramsWithWrite 3 >>= opMult
    3  -> paramsWithWrite 1 >>= opInput
    4  -> params 1 >>= opOutput
    5  -> params 2 >>= opJmpIfTrue
    6  -> params 2 >>= opJmpIfFalse
    7  -> paramsWithWrite 3 >>= opLessThan
    8  -> paramsWithWrite 3 >>= opEq

    99 -> opHalt
stepState_ Halted = return ()
stepState_ RequestInput = do
  inputQueue <- gets _inputQueue
  if null inputQueue then return () else runCallback "RequestInput"
stepState_ Outputing = do
  outputQueue <- gets _outputQueue
  if not $ null outputQueue then return () else runCallback "Outputing"

runCallback :: Monad m => String -> Program m ()
runCallback for = do
  mcallback <- gets _callback
  case mcallback of
    Nothing -> error $ "Missing callback for " ++ for
    Just callback -> do
      modify $ \intcode -> intcode {_state = Running, _callback = Nothing}
      callback

step :: Monad m => Program m Intcode
step = step_ >> get

runProgram :: Monad m => Program m Intcode
runProgram = iterateUntil (not . isRunning) step

runProgramIO :: ProgramIO Intcode
runProgramIO = iterateUntil isHalted $ do
  afterStop <- runProgram
  case _state afterStop of
    RequestInput -> do
      input <- liftIO (read <$> prompt "Input:  ")
      pushInput input
    Outputing    -> do
      output <- popOutput
      liftIO $ putStrLn $ "Output: " ++ show output
    _            -> pure ()

  get

-- Operations --
type Operation m = Monad m => [Param] -> Program m ()

opAdd :: Operation m
opAdd [x,y,target] = setAt target (x + y) >> modFocus 4
opAdd _ = error "opAdd requires 3 parameters"

opMult :: Operation m
opMult [x,y,target] = setAt target (x * y) >> modFocus 4
opMult _ = error "opMult requires 3 parameters"

opInput :: Operation m
opInput [target] = do
  inputQueue <- gets _inputQueue

  if null inputQueue
  then modify $ \intcode -> intcode {_state = RequestInput, _callback = Just $ inputCallback target}
  else inputCallback target
  where
    inputCallback :: Monad m => Index -> Program m ()
    inputCallback target = do
      inputQueue <- gets _inputQueue
      setAt target $ head inputQueue
      modify $ \intcode -> intcode {_inputQueue = tail inputQueue}
      modFocus 2
opInput _ = error "opInput requires 1 parameter"

opOutput :: Operation m
opOutput [value] = modify $ \intcode -> intcode 
  { _state = Outputing
  , _outputQueue = _outputQueue intcode ++ [value]
  , _callback = Just $ modFocus 2
  }
opOutput _ = error "opOutput requires 1 parameter"

opJmpIfTrue :: Operation m
opJmpIfTrue [0,_]     = modFocus 3
opJmpIfTrue [_,focus] = setFocus focus
opJmpIfTrue _ = error "opJmpIfTrue requires 2 parameters"

opJmpIfFalse :: Operation m
opJmpIfFalse [0,focus] = setFocus focus
opJmpIfFalse [_,_]     = modFocus 3
opJmpIfFalse _ = error "opJmpIfFalse requires 2 parameters"

opLessThan :: Operation m
opLessThan [x,y,target] = do
  bool (setAt target 0) (setAt target 1) $ x < y
  modFocus 4
opLessThan _ = error "opJmpIfFalse requires 3 parameters"

opEq :: Operation m
opEq [x,y,target] = do
  bool (setAt target 0) (setAt target 1) $ x == y
  modFocus 4
opEq _ = error "opEq requires 3 parameters"

opHalt :: Monad m => Program m ()
opHalt = modify $ \intcode -> intcode {_state = Halted}

-- Helpers --

isRunning :: Intcode -> Bool
isRunning = (==) Running . _state

isHalted :: Intcode -> Bool
isHalted = (==) Halted . _state

parseParamModes :: Int -> [ParamMode]
parseParamModes 0 = repeat Position
parseParamModes n =
  (case n `mod` 10 of
    0 -> Position
    1 -> Immediate
  ) : parseParamModes (n `div` 10)