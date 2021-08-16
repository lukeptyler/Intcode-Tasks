{-# LANGUAGE RankNTypes #-}
module Task03.Intcode.Types where

import           Data.Array                (Array)
import qualified Data.Array                as A

import           Control.Monad.Trans.State (StateT)

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

type Operation m = Monad m => [Param] -> Program m ()