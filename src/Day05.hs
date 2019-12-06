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
part2 input = readOutputs $ runProgram (mkProgram [] input) [5]

problem :: Problem In Out
problem =
  Problem "src/day05.txt" parser part1Examples part1 part2Examples part2
  where
    part1Examples = [ ("3,0,4,0,99", V.singleton 1)
                    , ("1002,4,3,4,33", V.empty)
                    ]

    part2Examples = [ ("3,9,8,9,10,9,4,9,99,-1,8", V.singleton 0)
                    , ("3,9,7,9,10,9,4,9,99,-1,8", V.singleton 1)
                    , ("3,3,1108,-1,8,3,4,3,99", V.singleton 0)
                    , ("3,3,1107,-1,8,3,4,3,99", V.singleton 1)
                    , ("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9", V.singleton 1)
                    , ("3,3,1105,-1,9,1101,0,0,12,4,12,99,1", V.singleton 1)
                    , ("3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99", V.singleton 999)
                    ]
