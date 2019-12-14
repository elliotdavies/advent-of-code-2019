module Day05
  ( problem
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text       as Text
import qualified Data.Vector     as V
import           Prelude
import           Problem
import           Utils.Intcode   (Memory, Program, ProgramState (..), mkProgram,
                                  parseMemory, runProgram)

type In = Memory
type Out = [Int]

parser :: Parser In
parser = parseMemory

part1 :: Solution In Out
part1 = runUntilHalt 1 [] . mkProgram []

runUntilHalt :: Int -> Out -> Program -> Out
runUntilHalt input outputs program =
  case runProgram program of
    Halted _                -> outputs
    Await continue          -> runUntilHalt input outputs $ continue input
    Yield (output,program') -> runUntilHalt input (outputs ++ [output]) program'

part2 :: Solution In Out
part2 = runUntilHalt 2 [] . mkProgram []

problem :: Problem In Out
problem =
  Problem "src/day05.txt" parser part1Examples part1 part2Examples part2
  where
    part1Examples = [ ("3,0,4,0,99", [1])
                    , ("1002,4,3,4,33", [])
                    ]

    part2Examples = [ ("3,9,8,9,10,9,4,9,99,-1,8", [0])
                    , ("3,9,7,9,10,9,4,9,99,-1,8", [1])
                    , ("3,3,1108,-1,8,3,4,3,99", [0])
                    , ("3,3,1107,-1,8,3,4,3,99", [1])
                    , ("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9", [1])
                    , ("3,3,1105,-1,9,1101,0,0,12,4,12,99,1", [1])
                    , ("3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99", [999])
                    ]
