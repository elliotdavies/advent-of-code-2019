module Day01
  ( problem
  ) where

import qualified Data.Text   as Text
import           Prelude
import           Problem

type In = [Int]
type Out = Int

parser :: Parser In
parser = fmap read . lines . Text.unpack

part1 :: Solution In Out
part1 = sum . fmap calcFuel

calcFuel :: Int -> Int
calcFuel x = (x `div` 3) - 2

part2 :: Solution In Out
part2 = sum . fmap go
  where
    go x =
      let fuel = calcFuel x
       in if fuel <= 0 then 0 else fuel + go fuel

problem :: Problem In Out
problem =
  Problem "src/day01.txt" parser part1Examples part1 part2Examples part2
  where
    part1Examples = [("12", 2), ("14", 2), ("1969", 654), ("100756", 33583)]

    part2Examples = [("14", 2), ("1969", 966), ("100756", 50346)]
