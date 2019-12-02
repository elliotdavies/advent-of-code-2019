module Utils
  ( mkProgram
  , runProgram
  , readMemory
  , readOutput
  ) where

import qualified Data.Vector as V
import           Prelude


type Inputs = Maybe (Int, Int) -- (noun, verb)

type Memory = V.Vector Int

data Program = Program
  { inputs   :: Inputs
  , instrPtr :: Int
  , memory   :: Memory
  }

mkProgram :: Inputs -> Memory -> Program
mkProgram inputs initialMemory =
  let memory = initialMemory V.//
        case inputs of
          Just (noun, verb) -> [(1, noun), (2, verb)]
          Nothing           -> []

    in Program { inputs, instrPtr = 0, memory }


readMemory :: Program -> Memory
readMemory = memory

readOutput :: Program -> Int
readOutput = V.head . readMemory

-- Execute a program until it halts
runProgram :: Program -> Program
runProgram program@Program{..} =
  case valueAt instrPtr memory of
    1  -> runProgram $ Program inputs nextInstrPtr (operate (+))
    2  -> runProgram $ Program inputs nextInstrPtr (operate (*))
    99 -> program
    _  -> error $ "Unrecognised opcode: " ++ show instrPtr
  where
    operate op =
      let param1 = dereference (instrPtr + 1) memory
          param2 = dereference (instrPtr + 2) memory
          storeAddr = valueAt (instrPtr + 3) memory
      in  memory V.// [(storeAddr, param1 `op` param2)]

    nextInstrPtr = instrPtr + 4


-- Retrieve the value at the given memory address
valueAt :: Int -> Memory -> Int
valueAt = flip (V.!)

-- Treat the given memory address as a pointer and follow it
dereference :: Int -> Memory -> Int
dereference i mem = mem V.! (valueAt i mem)
