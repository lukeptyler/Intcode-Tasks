{-# LANGUAGE RankNTypes #-}
module Task09.Intcode.Types where

import           Data.IntMap               (IntMap)
import qualified Data.IntMap               as IM

import           Control.Monad.Trans.State (StateT)
import           Data.Functor.Identity     (Identity)

type Index   = Int
type Param   = Int
type Code    = IntMap Int
data State   = Running
             | Halted
             | RequestInput
             | Outputing
  deriving (Show, Eq)

data Intcode = Intcode 
  { _state :: State
  , _focus, _relBase :: Index
  , _inputQueue, _outputQueue :: [Int]
  , _callback :: forall m. Monad m => Maybe (ProgramT m ())
  , _code :: Code}
instance Show Intcode where
  show intcode = 
    "Intcode {" ++ show (_state intcode) ++ 
    " | " ++ show (_focus intcode) ++ "(" ++ show (_relBase intcode) ++ ")" ++
    " | " ++ show (_code intcode) ++ "}" ++
    (if null $ _inputQueue  intcode then "" else "\ninputQueue = "  ++ show (_inputQueue  intcode)) ++
    (if null $ _outputQueue intcode then "" else "\noutputQueue = " ++ show (_outputQueue intcode))

initIntcode :: [Int] -> Intcode
initIntcode codeData = Intcode 
  { _state = Running
  , _focus = 0
  , _relBase = 0
  , _inputQueue = []
  , _outputQueue = []
  , _callback = Nothing
  , _code = IM.fromList $ zip [0..length codeData-1] codeData
  }

type ProgramT  = StateT Intcode
type Program   = ProgramT Identity
type ProgramIO = ProgramT IO

type IntegrationT  s m = StateT s (ProgramT m)
type Integration   s   = IntegrationT s Identity
type IntegrationIO s   = IntegrationT s IO

data ParamMode = Immediate
               | Position
               | Relative
               | WritePosition
               | WriteRelative
  deriving (Show, Eq)

type Operation m = Monad m => [Param] -> ProgramT m ()