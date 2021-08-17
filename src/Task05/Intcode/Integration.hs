{-# LANGUAGE RankNTypes #-}
module Task05.Intcode.Integration where

import Control.Monad.Trans.State (execStateT, evalStateT,
                                  execState, evalState,
                                  get)
import Control.Monad.Loops       (iterateUntil)

import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class    (liftIO)
import Lib                       (prompt)

import Task05.Intcode.Types      (Integration, IntegrationT, IntegrationIO, State(..), Intcode(_state))
import Task05.Intcode.Program    (runUntilStop)
import Task05.Intcode.Accessors  (pushInput, popOutput, isHalted)

execIntegration :: Integration s a -> s -> Intcode -> s
execIntegration integration s intcode = evalState (execStateT integration s) intcode

evalIntegration :: Integration s a -> s -> Intcode -> a
evalIntegration integration s intcode = evalState (evalStateT integration s) intcode

execIntegrationT :: Monad m => IntegrationT s m a -> s -> Intcode -> m s
execIntegrationT integration s intcode = evalStateT (execStateT integration s) intcode

evalIntegrationT :: Monad m => IntegrationT s m a -> s -> Intcode -> m a
evalIntegrationT integration s intcode = evalStateT (evalStateT integration s) intcode

runIntegration :: Monad m => IntegrationT s m () -> IntegrationT s m () -> IntegrationT s m Intcode
runIntegration handleInput handleOutput = iterateUntil isHalted $ do
  afterStop <- lift runUntilStop
  case _state afterStop of
    RequestInput -> handleInput
    Outputing    -> handleOutput
    _            -> pure ()
  lift get

runIntegrationIO :: (Int -> IntegrationIO s ()) -> (Int -> IntegrationIO s ()) -> IntegrationIO s Intcode
runIntegrationIO handleInput handleOutput = runIntegration (inputIO >>= handleInput) (outputIO >>= handleOutput)
  where
    inputIO :: IntegrationIO s Int
    inputIO = do 
      input <- liftIO $ read <$> prompt "Input:  "
      lift $ pushInput input
      return input

    outputIO :: IntegrationIO s Int
    outputIO = do
      output <- lift $ popOutput
      liftIO $ putStrLn $ "Output: " ++ show output
      return output

{-
testIntegration :: Integration [Int] IO String
testIntegration = do
  ic <- runIntegrationIO handleInput handleOutput

  liftIO $ print ic

  show <$> get
  where
    handleInput :: Int -> Integration [Int] IO ()
    handleInput input = modify (++ [input])

    handleOutput :: Int -> Integration [Int] IO ()
    handleOutput output = modify (++ [output])
-}