module Task03.Intcode.Program where

import Control.Monad.Trans.State (get, gets, modify)
import Control.Monad.Loops       (iterateUntil)
import Control.Monad.IO.Class    (liftIO)

import Lib                       (prompt)

import Task03.Intcode.Types      (State(..), Intcode(..),
                                  Program, ProgramIO)
import Task03.Intcode.Accessors  (opcode,
                                  params, paramsWithWrite,
                                  isRunning, isHalted,
                                  pushInput, popOutput)
import Task03.Intcode.Operations

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