module Utils.Intcode
  ( Program
  , mkProgram
  , runProgram
  , readMemory
  , readOutputs
  ) where

import           Data.Digits (digits, unDigits)
import qualified Data.Map    as Map
import qualified Data.Vector as V
import           Prelude     hiding (read, Ordering(..))

import Debug.Trace (traceShowId)


type Memory = V.Vector Int
type Outputs = V.Vector Int

data Program = Program
  { instrPtr :: Int
  , memory   :: Memory
  , outputs  :: Outputs
  }
  deriving (Show)

-- Construct a new program, optionally overriding some of the memory locations
mkProgram :: [(Int, Int)] -> Memory -> Program
mkProgram overrides initialMemory =
  Program
    { instrPtr = 0
    , memory   = initialMemory V.// overrides
    , outputs  = V.empty
    }

readMemory :: Program -> Memory
readMemory = memory

readOutputs :: Program -> Outputs
readOutputs = outputs

data Mode = Position | Immediate
  deriving (Show)

mode :: Int -> Mode
mode 0 = Position
mode 1 = Immediate
mode n = error $ "Unrecognised mode: " ++ show n

data OpCode
  = Add
  | Mul
  | In
  | Out
  | JmpNZ
  | JmpZ
  | LT
  | Eq
  | Halt
  deriving (Show, Enum)

opCodeMap :: Map.Map Int OpCode
opCodeMap = Map.insert 99 Halt $ Map.fromList $ zip [1..] [Add ..]

opCode :: Int -> OpCode
opCode i =
  case Map.lookup i opCodeMap of
    Just code -> code
    Nothing   -> error $ "Unrecognised opcode: " ++ show i

parseOpCode :: Int -> (OpCode, [Mode])
parseOpCode i =
  let digits' = reverse $ digits 10 i
      code   = unDigits 10 . reverse . take 2 $ digits'
      modes  = fmap mode . drop 2 $ digits'
   in (opCode code, modes ++ (repeat Position))

-- Execute a program until it halts. Accepts a program and a list of inputs to
-- be read. Returns the final state including a list of outputs
runProgram :: Program -> [Int] -> Program
runProgram program@Program{..} inputs =
  let (code, modes) = parseOpCode (valueAt instrPtr memory)

      params n = zip [instrPtr + 1 .. instrPtr + n] modes

      (program', inputs') = case code of
        Add ->
          let [p1,p2,p3] = params 3
              memory' = write (read p1 + read p2) p3
           in (program { instrPtr = instrPtr + 4, memory = memory' }, inputs)

        Mul ->
          let [p1,p2,p3] = params 3
              memory' = write (read p1 * read p2) p3
           in (program { instrPtr = instrPtr + 4, memory = memory' }, inputs)

        In ->
          let memory' = write (head inputs) (instrPtr + 1, Position)
           in (program { instrPtr = instrPtr + 2, memory = memory' }, tail inputs)

        Out ->
          let [p1] = params 1
              outputs' = V.snoc outputs $ read p1
           in (program { instrPtr = instrPtr + 2, outputs = outputs' }, inputs)

        JmpNZ ->
          let [p1,p2] = params 2
              instrPtr' = if read p1 /= 0 then read p2 else instrPtr + 3
           in (program { instrPtr = instrPtr' }, inputs)

        JmpZ ->
          let [p1,p2] = params 2
              instrPtr' = if read p1 == 0 then read p2 else instrPtr + 3
           in (program { instrPtr = instrPtr' }, inputs)

        LT ->
          let [p1,p2,p3] = params 3
              memory' = write (if read p1 < read p2 then 1 else 0) p3
           in (program { instrPtr = instrPtr + 4, memory = memory' }, inputs)

        Eq ->
          let [p1,p2,p3] = params 3
              memory' = write (if read p1 == read p2 then 1 else 0) p3
           in (program { instrPtr = instrPtr + 4, memory = memory' }, inputs)

        Halt ->
          (program, inputs)

  in case code of
       Halt -> program'
       _    -> runProgram program' inputs'

  where
    read (i, m) =
      case m of
        Position  -> dereference i memory
        Immediate -> valueAt i memory

    write val (i, m) =
      case m of
        Position -> memory V.// [(valueAt i memory, val)]
        Immediate -> error $ "Write instructions should never be immediate"

-- Retrieve the value at the given memory address
valueAt :: Int -> Memory -> Int
valueAt = flip (V.!)

-- Treat the given memory address as a pointer and follow it
dereference :: Int -> Memory -> Int
dereference i mem = mem V.! (valueAt i mem)