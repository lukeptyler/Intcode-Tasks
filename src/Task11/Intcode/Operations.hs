module Task11.Intcode.Operations where

import Control.Monad.Trans.State (gets, modify)

import Data.Bool                 (bool)

import Task11.Intcode.Types      (Intcode(..), ProgramT,
                                  Index, State(..), Operation)
import Task11.Intcode.Accessors  (setAt,
                                  modFocus, setFocus)

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
    inputCallback :: Monad m => Index -> ProgramT m ()
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

opAdjRelBase :: Operation m
opAdjRelBase [x] = do
  modify $ \intcode -> intcode {_relBase = _relBase intcode + x}
  modFocus 2
opAdjRelBase _ = error "opAdjRelBase requires 1 parameter"

opHalt :: Monad m => ProgramT m ()
opHalt = modify $ \intcode -> intcode {_state = Halted}