{-# LANGUAGE RankNTypes #-}
module Task04.Intcode 
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

import Task04.Intcode.Types
import Task04.Intcode.Accessors
import Task04.Intcode.Operations
import Task04.Intcode.Program