module Day02
  ( problem
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text       as Text
import qualified Data.Vector     as V
import           Prelude
import           Problem
import           Utils.Intcode   (Memory, Program, ProgramState (..), mkProgram,
                                  parseMemory, readMemory, runProgram)


type In = (Memory, [(Int, Int)])
type Out = [Int]


parser :: Parser In
parser input =
  let memory:inputs = Text.lines input
   in (parseMemory memory, parseInputs inputs)
  where
    parseInputs []          = []
    parseInputs [noun,verb] = [(1, parseToInt noun), (2, parseToInt verb)]

    parseToInt = read . Text.unpack


part1 :: Solution In Out
part1 (memory, overrides) =
  Map.elems $ readMemory $ runUntilHalted (mkProgram overrides memory)

runUntilHalted :: Program -> Program
runUntilHalted program =
  case runProgram program of
    Halted p -> p
    Await _  -> error "Not expecting input"
    Yield _  -> error "Not expecting output"

part2 :: Solution In Out
part2 (memory, _) =
  foldr check [] [[(1,noun),(2, verb)] | noun <- [0..99], verb <- [0..99]]
  where
    check overrides acc =
      let program' = runUntilHalted (mkProgram overrides memory)
       in if readMemory program' Map.! 0 == target
           then fmap snd overrides
           else acc

    target = 19690720


problem :: Problem In Out
problem =
  Problem "src/day02.txt" parser part1Examples part1 part2Examples part2
  where
    part1Examples =
      [ ("1,9,10,3,2,3,11,0,99,30,40,50", [3500,9,10,70,2,3,11,0,99,30,40,50])
      , ("1,0,0,0,99"                   , [2,0,0,0,99])
      , ("2,3,0,3,99"                   , [2,3,0,6,99])
      , ("2,4,4,5,99,0"                 , [2,4,4,5,99,9801])
      , ("1,1,1,4,99,5,6,0,99"          , [30,1,1,4,2,5,6,0,99])
      ]

    part2Examples = []
