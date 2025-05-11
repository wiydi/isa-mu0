module MU0 (
    Word12, mkWord12, getWord12,
    MU0_Instruction, 
    MU0_Register,
    MU0_Memory, mkMU0_Memory, getMU0_Memory,
    MU0_State, mu0_fetch, mu0_decode, mu0_execute, mu0_ISA
) where

import Data.Bits
import Data.Word
import Data.Array


newtype Word12 = Word12 Word16 deriving (Eq, Ord, Bounded, Ix, Num, Show)

--x .&. 0xFFF--
mkWord12 :: Word16 -> Maybe Word12
mkWord12 x 
    | x > 0xFFF = Nothing
    | otherwise = Just (Word12 x)

getWord12 :: Word12 -> Word16
getWord12 (Word12 x) = x


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
                                   acc :: Word16
                                 }

newtype MU0_Memory = MU0_Memory (Array Word12 Word16) deriving (Show)

--constraint : max length is 2^12--
mkMU0_Memory :: Array Word12 Word16 -> MU0_Memory
mkMU0_Memory arr = MU0_Memory $ array (0, 4096-1) $ [(Word12 i, if inRange (bounds arr) (Word12 i) then arr ! (Word12 i) else 0x0) | i<-[0..4095]]

getMU0_Memory :: MU0_Memory -> Array Word12 Word16
getMU0_Memory (MU0_Memory x) = x


data MU0_State = MU0_State { register :: MU0_Register,
                             memory :: MU0_Memory
                           }

mu0_step :: MU0_State -> MU0_State
mu0_step st = st {register = (register st) {pc = (pc $ register $ st) + 1}}

mu0_fetch :: MU0_State -> MU0_Instruction
mu0_fetch st = MU0_Instruction $ (getMU0_Memory $ memory $ st) ! (pc $ register $ st)

mu0_decode :: MU0_Instruction -> Maybe MU0_ParsedInstruction
mu0_decode (MU0_Instruction x)
    | opcode == 0x0 = operand >>= (Just . LDA)
    | opcode == 0x1 = operand >>= (Just . STO)
    | opcode == 0x2 = operand >>= (Just . ADD)
    | opcode == 0x3 = operand >>= (Just . SUB)
    | opcode == 0x4 = operand >>= (Just . JMP)
    | opcode == 0x5 = operand >>= (Just . JGE)
    | opcode == 0x6 = operand >>= (Just . JNE)
    | opcode == 0x7 = Just STP
    | otherwise = Nothing
    where opcode = shift (x .&. 0xF000) (-12)
          operand = mkWord12 $ x .&. 0x0FFF 

mu0_execute :: MU0_ParsedInstruction -> Maybe (MU0_State -> MU0_State)
mu0_execute (LDA op) = Just $ mu0_step . \st -> (st {register = ((register st) {acc = (getMU0_Memory $ memory $ st) ! op })})
mu0_execute (STO op) = Just $ mu0_step . \st -> (st {memory = MU0_Memory ((getMU0_Memory $ memory $ st) // [(op, acc $ register $ st)])})
mu0_execute (ADD op) = Just $ mu0_step . \st -> (st {register = ((register st) {acc = (acc $ register $ st) + (getMU0_Memory $ memory $ st) ! op})})
mu0_execute (SUB op) = Just $ mu0_step . \st -> (st {register = ((register st) {acc = (acc $ register $ st) - (getMU0_Memory $ memory $ st) ! op})})
mu0_execute (JMP op) = Just $ mu0_step . \st -> (st {register = ((register st) {pc = op})})
mu0_execute (JGE op) = Just $ mu0_step . \st -> if (acc $ register $ st) >= 0 then (st {register = ((register st) {pc = op})}) else st
mu0_execute (JNE op) = Just $ mu0_step . \st -> if (acc $ register $ st) /= 0 then (st {register = ((register st) {pc = op})}) else st
mu0_execute STP = Nothing



mu0_ISA :: MU0_Instruction -> Maybe (MU0_State -> MU0_State) 
mu0_ISA ir = (mu0_decode ir) >>= mu0_execute
