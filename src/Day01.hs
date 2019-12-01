module Day01
  ( problem
  ) where

import           Data.Monoid (Sum (..))
import           Data.Text   (Text)
import qualified Data.Text   as Text
import           Debug.Trace (traceShowId)
import           Prelude
import           Problem

type In = [Int]
type Out = Int

parser :: Parser In
parser = fmap read . lines . Text.unpack

part1 :: Solution In Out
part1 = getSum . foldMap fuel

fuel :: Int -> Sum Int
fuel x = Sum $ (x `div` 3) - 2

part2 :: Solution In Out
part2 = getSum . foldMap go
  where
    go :: Int -> Sum Int
    go x =
      let Sum f = fuel x
       in if f <= 0 then 0 else Sum f <> go f

problem :: Problem In Out
problem =
  Problem "src/day01.txt" parser part1Examples part1 part2Examples part2
  where
    part1Examples = [("12", 2), ("14", 2), ("1969", 654), ("100756", 33583)]

    part2Examples = [("14", 2), ("1969", 966), ("100756", 50346)]
