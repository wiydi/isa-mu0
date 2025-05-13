module ISA (
    Word12, 
    MU0_Instruction, 
    MU0_Register, 
    MU0_Memory, mkMU0_Memory, getMU0_Memory,
    MU0_State(..), mu0_fetch, mu0_decode, mu0_execute, mu0_ISA, 
    mu0_cycle, mu0_getStateList, mu0Peek, mu0_getState, 
    readWordAt, readaccAt, readpcAt 
) where

import Data.Bits
import Data.Word
import Data.Array
import Data.Int

type Word12 = Word16

data MU0_ParsedInstruction = LDA Word12
                           | STO Word12
                           | ADD Word12
                           | SUB Word12
                           | JMP Word12
                           | JGE Word12
                           | JNE Word12
                           | STP
                           deriving (Show)

--IR register--
newtype MU0_Instruction = MU0_Instruction Word16 deriving (Show)

data MU0_Register = MU0_Register { pc :: Word12,
                                   acc :: Int32 
                                 } deriving(Show)

newtype MU0_Memory = MU0_Memory (Array Word12 Word16) deriving (Show)

--constraint : max length is 2^12--
mkMU0_Memory :: Array Word12 Word16 -> MU0_Memory
mkMU0_Memory arr = MU0_Memory $ array (0, 4095) $ [(i, if inRange (bounds arr) i then arr ! i else 0x0) | i<-[0..4095]]

getMU0_Memory :: MU0_Memory -> Array Word12 Word16
getMU0_Memory (MU0_Memory x) = x


data MU0_State = MU0_State { register :: MU0_Register,
                             memory :: MU0_Memory
                           } deriving(Show)

mu0_step :: MU0_State -> MU0_State
mu0_step st = st {register = (register st) {pc = (pc $ register $ st) + 1}}

mu0_fetch :: MU0_State -> MU0_Instruction
mu0_fetch st = MU0_Instruction $ (getMU0_Memory $ memory $ st) ! (pc $ register $ st)

mu0_decode :: MU0_Instruction -> Maybe MU0_ParsedInstruction
mu0_decode (MU0_Instruction x)
    | opcode == 0x0 = (Just . LDA) operand
    | opcode == 0x1 = (Just . STO) operand
    | opcode == 0x2 = (Just . ADD) operand
    | opcode == 0x3 = (Just . SUB) operand
    | opcode == 0x4 = (Just . JMP) operand
    | opcode == 0x5 = (Just . JGE) operand
    | opcode == 0x6 = (Just . JNE) operand
    | opcode == 0x7 = Just STP
    | otherwise = Nothing
    where opcode = shift (x .&. 0xF000) (-12)
          operand = x .&. 0x0FFF 

mu0_execute :: MU0_ParsedInstruction -> Maybe (MU0_State -> MU0_State)
mu0_execute (LDA op) = Just $ mu0_step . \st -> (st {register = ((register st) {acc = fromIntegral $ (getMU0_Memory $ memory $ st) ! op })})
mu0_execute (STO op) = Just $ mu0_step . \st -> (st {memory = MU0_Memory ((getMU0_Memory $ memory $ st) // [(op, fromIntegral $ acc $ register $ st)])})
mu0_execute (ADD op) = Just $ mu0_step . \st -> (st {register = ((register st) {acc = (acc $ register $ st) + (fromIntegral $ (getMU0_Memory $ memory $ st) ! op)})})
mu0_execute (SUB op) = Just $ mu0_step . \st -> (st {register = ((register st) {acc = (acc $ register $ st) - (fromIntegral $ (getMU0_Memory $ memory $ st) ! op)})})
mu0_execute (JMP op) = Just $  \st -> (st {register = ((register st) {pc = op})})
mu0_execute (JGE op) = Just $  \st -> if (acc $ register $ st) >= 0 then (st {register = ((register st) {pc = op})}) else mu0_step st
mu0_execute (JNE op) = Just $  \st -> if (acc $ register $ st) /= 0 then (st {register = ((register st) {pc = op})}) else mu0_step st
mu0_execute STP = Nothing


mu0_ISA :: MU0_Instruction -> Maybe (MU0_State -> MU0_State) 
mu0_ISA ir = (mu0_decode ir) >>= mu0_execute

--Interface functions--

mu0_cycle :: MU0_State -> Maybe MU0_State
mu0_cycle st = (mu0_ISA . mu0_fetch) st >>= (\f -> Just $ f $ st)

mu0_getStateList :: MU0_State -> [Maybe MU0_State]
mu0_getStateList st = stateList
                    where stateList = (Just st):map (\x -> x >>= mu0_cycle) stateList

mu0Peek :: Word12 -> MU0_State -> Word16
mu0Peek addr st  = (getMU0_Memory $ memory $ st) ! addr

mu0_getState :: [Word16] -> Maybe MU0_State
mu0_getState ms
    | size > 4096 = Nothing
    | size == 0   = Just $ MU0_State {register=rI,memory=mkMU0_Memory $ array (1,0) []}
    | otherwise   = let endIx = fromIntegral $ (size-1)
                        lst = zip [0..endIx] ms
                        mI = mkMU0_Memory $ array (0,endIx) lst
                    in Just $ MU0_State {register=rI,memory=mI}
    where size = length ms
          rI = MU0_Register {pc=0, acc=0} 

readWordAt :: Int -> Word16 -> [Maybe MU0_State] -> Maybe Word16
readWordAt x y lst = lst !! x >>= Just . (mu0Peek y)

readaccAt :: Int -> [Maybe MU0_State] -> Maybe Int32 
readaccAt x lst = lst !! x >>= Just . acc . register

readpcAt :: Int -> [Maybe MU0_State] -> Maybe Word16
readpcAt x lst = lst !! x >>= Just . pc . register 
