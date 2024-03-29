module Day09
  ( problem
  ) where

import           Data.Text     (Text)
import qualified Data.Vector   as V
import           Prelude
import           Problem
import           Utils.Intcode (Program, ProgramState (..), mkProgram,
                                parseMemory, runProgram)

type In = Program
type Out = [Int]

parser :: Parser In
parser = mkProgram [] . parseMemory

part1 :: Solution In Out
part1 = collectOutputs 1

collectOutputs :: Int -> Program -> [Int]
collectOutputs input = go [] . runProgram
  where
    go outputs = \case
      Halted _ -> outputs
      Await continue -> go outputs $ runProgram (continue input)
      Yield (output, program) -> go (outputs ++ [output]) $ runProgram program

part2 :: Solution In Out
part2 = collectOutputs 2

problem :: Problem In Out
problem =
  Problem "src/day09.txt" parser part1Examples part1 part2Examples part2
  where
    part1Examples =
      [ ("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99", [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99])
      , ("1102,34915192,34915192,7,4,7,99,0", [1219070632396864])
      , ("104,1125899906842624,99", [1125899906842624])
      ]

    part2Examples = []
