module Day$DAY
  ( problem
  ) where

import qualified Data.Text as Text
import           Prelude
import           Problem

type In = Text.Text
type Out = Int

parser :: Parser In
parser input = ""

part1 :: Solution In Out
part1 input = 0

part2 :: Solution In Out
part2 input = 0

problem :: Problem In Out
problem =
  Problem "src/day$DAY.txt" parser part1Examples part1 part2Examples part2
  where
    part1Examples = []

    part2Examples = []
