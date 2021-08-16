{-# LANGUAGE RankNTypes #-}
module Task03.Intcode 
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

import Task03.Intcode.Types
import Task03.Intcode.Accessors
import Task03.Intcode.Operations
import Task03.Intcode.Program