module Task11.Core
  ( part1
  , part2
  ) where

import           Control.Monad.Trans.State (StateT, evalStateT, execStateT, runState, get, gets, modify, put)
import           Control.Monad.Loops       (iterateUntil)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.IO.Class    (liftIO)

import           Data.IntMap               (IntMap)
import qualified Data.IntMap               as IM

import           Data.Set                  (Set)
import qualified Data.Set                  as S

import           Text.Printf               (printf)

import           Lib                       (inputFile)
import           Task11.Intcode            (initIntcode, 
                                            execProgram, evalProgram, evalProgramT, execProgramT, runProgramIO,
                                            pushInput, pushInputList, popOutput)
import           Task11.Intcode.Program    (step)
import           Task11.Intcode.Types      (Intcode(..), State(..), ProgramT)


part1 :: IO ()
part1 = do
  intcode <- initIntcode <$> inputFile "src/Task11/input.txt"
  packet <- evalStateT runNetwork $ initComputers 50 intcode

  putStrLn ""
  print $ _y packet

part2 :: IO ()
part2 = do
  intcode <- initIntcode <$> inputFile "src/Task11/input.txt"

  let initialNAT = NAT Empty False S.empty
      initialComputers = initComputers 50 intcode

  packet <- evalStateT runNATNetwork (initialNAT, initialComputers)

  putStrLn ""
  print $ _y packet

data Packet = Empty
            | Partial1 Int
            | Partial2 Int Int
            | Packet {_target, _x, _y :: Int}
  deriving (Eq)
instance Show Packet where
  show Empty          = "Empty"
  show (Partial1 t)   = show t ++ " PARTIAL"
  show (Partial2 t x) = show t ++ " (" ++ show x ++ " PARTIAL)"
  show (Packet t x y) = show t ++ " (" ++ show x ++ " " ++ show y ++ ")"

addToPacket :: Int -> Packet -> Packet
addToPacket t Empty          = Partial1 t
addToPacket x (Partial1 t)   = Partial2 t x
addToPacket y (Partial2 t x) = Packet t x y

isPacketComplete :: Packet -> Bool
isPacketComplete (Packet _ _ _) = True
isPacketComplete _              = False

type Computer = (Packet, Intcode)

initComputers :: Int -> Intcode -> IntMap Computer
initComputers count intcode = IM.fromAscList $ map (\addr -> (addr, (Empty,intcode{_inputQueue = [addr]}))) [0..count-1]

type Network = StateT (IntMap Computer) IO

runNetwork :: Network Packet
runNetwork = iterateUntil (\packet -> isPacketComplete packet && _target packet == 255) stepNetwork
  where
    stepNetwork :: Network Packet
    stepNetwork = do
      stepComputers
      packet <- handleOutput
      handleInput
      return packet

    stepComputers :: Network ()
    stepComputers = modify $ IM.map (\(p, intcode) -> (p, execProgram step intcode))

    handleOutput :: Network Packet
    handleOutput = do
      computers <- get

      handleComputerOutputs $ IM.assocs computers
      where
        handleComputerOutputs :: [(Int, Computer)] -> Network Packet
        handleComputerOutputs [] = return Empty
        handleComputerOutputs ((addr, (p, i@Intcode{_state = Outputing})) : ls) = do
          let (output, intcode') = runState popOutput i :: (Int, Intcode)
              packet' = addToPacket output p

          if isPacketComplete packet'
          then do
            liftIO $ putStrLn $ show addr ++ " -> " ++ show packet'

            modify $ IM.update (const $ Just (Empty, intcode')) addr
            modify $ IM.update (\(targetP, targetI) -> Just (targetP, execProgram (pushInputList [_x packet', _y packet']) targetI)) $ _target packet'
            
            if _target packet' == 255
            then handleComputerOutputs ls >> return packet'
            else handleComputerOutputs ls
          else do
            modify $ IM.update (const $ Just (packet', intcode')) addr

            handleComputerOutputs ls
        handleComputerOutputs (_:ls) = handleComputerOutputs ls

    handleInput :: Network ()
    handleInput = do
      computers <- get

      handleComputerInputs $ IM.assocs computers
      where
        handleComputerInputs :: [(Int, Computer)] -> Network ()
        handleComputerInputs [] = return ()
        handleComputerInputs ((addr, (p, i@Intcode{_state = RequestInput, _inputQueue = []})) : ls) = do
          modify $ IM.update (const $ Just (p, execProgram (pushInput (-1)) i)) addr
          handleComputerInputs ls
        handleComputerInputs (_:ls) = handleComputerInputs ls

data NAT = NAT {_packet :: Packet, _hasSent :: Bool, _waiting :: Set Int}
type NATNetwork = StateT (NAT, IntMap Computer) IO

runNATNetwork :: NATNetwork Packet
runNATNetwork = iterateUntil (/= Empty) stepNetwork
  where
    stepNetwork :: NATNetwork Packet
    stepNetwork = do
      stepComputers
      handleOutput
      handleInput
      handleNAT

    stepComputers :: NATNetwork ()
    stepComputers = modify $ \(nat,computers) -> (nat, IM.map (\(p, intcode) -> (p, execProgram step intcode)) computers)

    handleOutput :: NATNetwork ()
    handleOutput = do
      computers <- gets snd

      packet <- handleComputerOutputs $ IM.assocs computers

      if packet /= Empty
      then modify $ \(nat, computers) -> (nat {_packet = packet, _hasSent = _hasSent nat && packet == _packet nat}, computers)
      else return ()
      where
        handleComputerOutputs :: [(Int, Computer)] -> NATNetwork Packet
        handleComputerOutputs [] = return Empty
        handleComputerOutputs ((addr, (p, i@Intcode{_state = Outputing})) : ls) = do
          modify $ \(nat, computers) -> (nat {_waiting = S.delete addr $ _waiting nat}, computers)

          let (output, intcode') = runState popOutput i :: (Int, Intcode)
              packet' = addToPacket output p

          if isPacketComplete packet'
          then do
            liftIO $ putStrLn $ show addr ++ " -> " ++ show packet'

            modify $ \(nat, computers) -> (nat, IM.update (const $ Just (Empty, intcode')) addr computers)
            modify $ \(nat, computers) -> (nat {_waiting = S.delete (_target packet') $ _waiting nat}, IM.update (\(targetP, targetI) -> Just (targetP, execProgram (pushInputList [_x packet', _y packet']) targetI)) (_target packet') computers)
            
            if _target packet' == 255
            then handleComputerOutputs ls >> return packet'
            else handleComputerOutputs ls
          else do
            modify $ \(nat, computers) -> (nat, IM.update (const $ Just (packet', intcode')) addr computers)

            handleComputerOutputs ls
        handleComputerOutputs (_:ls) = handleComputerOutputs ls

    handleInput :: NATNetwork ()
    handleInput = do
      computers <- gets snd

      handleComputerInputs $ IM.assocs computers
      where
        handleComputerInputs :: [(Int, Computer)] -> NATNetwork ()
        handleComputerInputs [] = return ()
        handleComputerInputs ((addr, (p, i@Intcode{_state = RequestInput})) : ls)
          | null $ _inputQueue i = do
              modify $ \(nat, computers) -> (nat {_waiting = S.insert addr $ _waiting nat}, IM.update (const $ Just (p, execProgram (pushInput (-1)) i)) addr computers)
              handleComputerInputs ls
          | otherwise = do
              modify $ \(nat, computers) -> (nat, computers)
              handleComputerInputs ls
        handleComputerInputs (_:ls) = handleComputerInputs ls

    handleNAT :: NATNetwork Packet
    handleNAT = do
      (nat, computers) <- get

      if _packet nat /= Empty && S.size (_waiting nat) == 50 && emptyInputQueues (map snd $ IM.elems computers)
      then do
        let done = _hasSent nat
            packet = _packet nat
        modify $ const $ (nat {_hasSent = True, _waiting = S.delete 0 $ _waiting nat}, IM.update (\(p,i) -> Just (p, i{_inputQueue = [_x packet, _y packet]})) 0 computers)

        if done
        then return $ _packet nat
        else return Empty
      else return Empty

emptyInputQueues :: [Intcode] -> Bool
emptyInputQueues [] = True
emptyInputQueues (i:is)
  | null (_inputQueue i) || _inputQueue i == [-1] = emptyInputQueues is
  | otherwise                                     = False




