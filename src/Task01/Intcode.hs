module Task01.Intcode 
  ( Intcode
  , Program
  , initIntcode
  , runUntilHalt
  , codeAt
  ) where

import           Data.Array                (Array)
import qualified Data.Array                as A

import           Control.Monad.Trans.State (StateT, gets, get, modify)
import           Control.Monad.Loops       (iterateUntil)
import           Control.Monad             (guard)

type Index   = Int
type Code    = Array Index Int
data State   = Running
             | Halted
  deriving (Show, Eq)

data Intcode = Intcode {_state :: State, _focus :: Index, _code :: Code}
  deriving (Show)

initIntcode :: [Int] -> Intcode
initIntcode codeData = Intcode Running 0 $ A.listArray (0,length codeData-1) codeData

type Program = StateT Intcode IO

setFocus :: Index -> Program ()
setFocus f' = modify $ \intcode -> intcode {_focus = f'}

modFocus :: Index -> Program ()
modFocus f' = modify $ \intcode -> intcode {_focus = f' + _focus intcode}

codeAt :: Index -> Program Int
codeAt index = do
  code <- gets _code
  return $ code A.! index

setAt :: Index -> Int -> Program ()
setAt i v = do
  code <- gets _code
  modify $ \intcode -> intcode {_code = code A.// [(i, v)]}

opcode :: Program Int
opcode = gets _focus >>= codeAt

args :: Int -> Program [Int]
args argCount = do
  focus <- gets _focus
  mapM codeAt [focus+1 .. focus+argCount]

step_ :: Program ()
step_ = do
  halted <- isHalted <$> get

  if halted then return () else do
    op <- opcode
    case op of
      1 -> do
        [x,y,target] <- args 3
        setAt target =<< (+) <$> codeAt x <*> codeAt y
        modFocus 4
      2 -> do
        [x,y,target] <- args 3
        setAt target =<< (*) <$> codeAt x <*> codeAt y
        modFocus 4
      99 -> modify (\intcode -> intcode {_state = Halted})

step :: Program Intcode
step = step_ >> get

runUntilHalt :: Program Intcode
runUntilHalt = iterateUntil isHalted step

isHalted :: Intcode -> Bool
isHalted = (==) Halted . _state