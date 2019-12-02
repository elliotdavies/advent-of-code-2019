module Utils
  ( Program(..)
  , runProgram
  ) where

import qualified Data.Vector as V
import           Prelude


type Memory = V.Vector Int

data Program = Program
  { pointer :: Int
  , memory  :: Memory
  }


-- Execute a program until it halts
runProgram :: Program -> Program
runProgram program@Program{pointer, memory} =
  case valueAt pointer memory of
    1  -> runProgram $ Program nextPointer (operate (+))
    2  -> runProgram $ Program nextPointer (operate (*))
    99 -> program
    _  -> error $ "Unrecognised opcode: " ++ show pointer
  where
    operate op =
      let operand1 = dereference (pointer + 1) memory
          operand2 = dereference (pointer + 2) memory
          store = valueAt (pointer + 3) memory
      in  memory V.// [(store, operand1 `op` operand2)]

    nextPointer = pointer + 4


-- Retrieve the value at the given memory location
valueAt :: Int -> Memory -> Int
valueAt = flip (V.!)

-- Treat the given memory location as a pointer and follow it
dereference :: Int -> Memory -> Int
dereference i mem = mem V.! (valueAt i mem)
