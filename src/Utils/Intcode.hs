module Utils.Intcode
  ( Program
  , ProgramState(..)
  , Memory
  , parseMemory
  , mkProgram
  , runProgram
  , readMemory
  ) where

import           Data.Digits      (digits, unDigits)
import qualified Data.Map.Strict  as Map
import qualified Data.Text        as Text
import           Prelude          hiding (read, Ordering(..))
import qualified Prelude          as Prelude


type Memory = Map.Map Int Int

data Program = Program
  { instrPtr      :: Int
  , relativeBase  :: Int
  , memory        :: Memory
  }
  deriving (Show)

data ProgramState
  = Halted Program
  | Await (Int -> Program)
  | Yield (Int, Program)

parseMemory :: Text.Text -> Memory
parseMemory = Map.fromList . zip [0..] . fmap (Prelude.read . Text.unpack) . Text.splitOn ","

-- Construct a new program, optionally overriding some of the memory locations
mkProgram :: [(Int, Int)] -> Memory -> Program
mkProgram overrides initialMemory =
  Program
    { instrPtr     = 0
    , relativeBase = 0
    , memory       = foldr (\(k,v) acc -> Map.insert k v acc) initialMemory overrides
    }

readMemory :: Program -> Memory
readMemory = memory

data Mode
  = Position
  | Immediate
  | Relative
  deriving (Show)

mode :: Int -> Mode
mode 0 = Position
mode 1 = Immediate
mode 2 = Relative
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
  | Rel
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

-- Execute a program until it halts, awaits input, or yields output
runProgram :: Program -> ProgramState
runProgram program@Program{..} =
  let (code, modes) = parseOpCode (valueAt instrPtr memory)

      params n = zip [instrPtr + 1 .. instrPtr + n] modes

   in case code of
        Add ->
          let [p1,p2,p3] = params 3
              memory' = write (read p1 + read p2) p3
           in runProgram $ program { instrPtr = instrPtr + 4, memory = memory' }

        Mul ->
          let [p1,p2,p3] = params 3
              memory' = write (read p1 * read p2) p3
           in runProgram $ program { instrPtr = instrPtr + 4, memory = memory' }

        In ->
          let [p1] = params 1
           in Await $ \input -> 
                let memory' = write input p1
                 in program { instrPtr = instrPtr + 2, memory = memory' }

        Out ->
          let [p1] = params 1
           in Yield (read p1, program { instrPtr = instrPtr + 2 })

        JmpNZ ->
          let [p1,p2] = params 2
              instrPtr' = if read p1 /= 0 then read p2 else instrPtr + 3
           in runProgram $ program { instrPtr = instrPtr' }

        JmpZ ->
          let [p1,p2] = params 2
              instrPtr' = if read p1 == 0 then read p2 else instrPtr + 3
           in runProgram $ program { instrPtr = instrPtr' }

        LT ->
          let [p1,p2,p3] = params 3
              memory' = write (if read p1 < read p2 then 1 else 0) p3
           in runProgram $ program { instrPtr = instrPtr + 4, memory = memory' }

        Eq ->
          let [p1,p2,p3] = params 3
              memory' = write (if read p1 == read p2 then 1 else 0) p3
           in runProgram $ program { instrPtr = instrPtr + 4, memory = memory' }

        Rel ->
          let [p1] = params 1
           in runProgram $ program { instrPtr = instrPtr + 2, relativeBase = relativeBase + read p1 }

        Halt ->
          Halted program

  where
    read (i, m) =
      case m of
        Position  -> dereference i memory
        Immediate -> valueAt i memory
        Relative  -> valueAt (valueAt i memory + relativeBase) memory

    write val (i, m) =
      case m of
        Position -> Map.insert (valueAt i memory) val memory
        Immediate -> error $ "Write instructions should never be immediate"
        Relative -> Map.insert (valueAt i memory + relativeBase) val memory

-- Retrieve the value at the given memory address
valueAt :: Int -> Memory -> Int
valueAt = Map.findWithDefault 0

-- Treat the given memory address as a pointer and follow it
dereference :: Int -> Memory -> Int
dereference i mem = valueAt (valueAt i mem) mem
