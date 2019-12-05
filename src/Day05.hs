module Day05
  ( problem
  ) where

import qualified Data.Text     as Text
import qualified Data.Vector   as V
import           Prelude
import           Problem
import           Utils.Intcode (Program, mkProgram, runProgram, readOutputs)

type In = V.Vector Int
type Out = V.Vector Int

parser :: Parser In
parser = V.fromList . fmap (read . Text.unpack) . Text.splitOn ","

part1 :: Solution In Out
part1 input = readOutputs $ runProgram (mkProgram [] input) [1]

part2 :: Solution In Out
part2 input = readOutputs $ runProgram (mkProgram [] V.empty) []

problem :: Problem In Out
problem =
  Problem "src/day05.txt" parser part1Examples part1 part2Examples part2
  where
    -- part1Examples = [ ("3,0,4,0,99", V.singleton 1)
    --                 , ("1002,4,3,4,33", V.empty)
    --                 ]

    part1Examples = []
    part2Examples = []
