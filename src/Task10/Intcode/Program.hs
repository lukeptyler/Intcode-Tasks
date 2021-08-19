module Task10.Intcode.Program where

import Control.Monad.Trans.State (execStateT, evalStateT,
                                  execState, evalState,
                                  get, gets, modify)
import Control.Monad.Loops       (iterateUntil)
import Control.Monad.IO.Class    (liftIO)
import Control.Monad             (void)

import Lib                       (prompt)

import Task10.Intcode.Types      (State(..), Intcode(..),
                                  Program, ProgramT, ProgramIO)
import Task10.Intcode.Accessors  (opcode,
                                  params, paramsWithWrite,
                                  isRunning, isHalted,
                                  pushInput, pushInputList, popOutput)
import Task10.Intcode.Operations

execProgram :: Program a -> Intcode -> Intcode
execProgram = execState

evalProgram :: Program a -> Intcode -> a
evalProgram = evalState

execProgramT :: Monad m => ProgramT m a -> Intcode -> m Intcode
execProgramT = execStateT

evalProgramT :: Monad m => ProgramT m a -> Intcode -> m a
evalProgramT = evalStateT

step_ :: Monad m => ProgramT m ()
step_ = do
  state <- gets _state
  stepState_ state
    
stepState_ :: Monad m => State -> ProgramT m ()
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
    9  -> params 1 >>= opAdjRelBase

    99 -> opHalt

    n -> do
      focus <- gets _focus
      error $ "invalid opcode: " ++ show n ++ " " ++ "Focus: " ++ show focus
stepState_ Halted = return ()
stepState_ RequestInput = do
  inputQueue <- gets _inputQueue
  if null inputQueue then return () else runCallback "RequestInput"
stepState_ Outputing = do
  outputQueue <- gets _outputQueue
  if not $ null outputQueue then return () else runCallback "Outputing"

runCallback :: Monad m => String -> ProgramT m ()
runCallback for = do
  mcallback <- gets _callback
  case mcallback of
    Nothing -> error $ "Missing callback for " ++ for
    Just callback -> do
      modify $ \intcode -> intcode {_state = Running, _callback = Nothing}
      callback

step :: Monad m => ProgramT m Intcode
step = step_ >> get

runUntilStop :: Monad m => ProgramT m Intcode
runUntilStop = iterateUntil (not . isRunning) step

runProgram :: Monad m => ProgramT m () -> ProgramT m () -> ProgramT m ()
runProgram handleInput handleOutput = void $ iterateUntil isHalted $ do
  afterStop <- runUntilStop
  case _state afterStop of
    RequestInput -> handleInput
    Outputing    -> handleOutput
    _            -> pure ()
  get

runProgramIO :: ProgramIO ()
runProgramIO = runProgram inputIO outputIO
  where
    inputIO :: ProgramIO ()
    inputIO = do
      input <- liftIO $ read <$> prompt "Input:  "
      pushInput input

    outputIO :: ProgramIO ()
    outputIO = do
      output <- popOutput
      liftIO $ putStrLn $ "Output: " ++ show output

runProgramASCII :: ProgramIO ()
runProgramASCII = runProgram inputASCII outputASCII
  where
    inputASCII :: ProgramIO ()
    inputASCII = do
      input <- liftIO $ prompt "Input: "
      pushInputList $ map fromEnum $ input ++ "\n"

    outputASCII :: ProgramIO ()
    outputASCII = do
      output <- popOutput
      if output <= 127
      then liftIO $ putChar $ toEnum output
      else liftIO $ print output