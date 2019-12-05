module Day04
  ( problem
  ) where

import           Data.Digits (digits)
import           Data.List   (group)
import qualified Data.Text   as Text
import           Prelude
import           Problem

type In = (Int, Int)
type Out = Int

parser :: Parser In
parser input =
  let (x:y:_) = fmap Text.unpack $ Text.lines input
   in (read x, read y)

part1 :: Solution In Out
part1 (min, max) = findMatching criteria [min..max]
  where
    criteria xs = increasing xs && any ((>= 2) . length) (group xs)

findMatching :: ([Int] -> Bool) -> [Int] -> Int
findMatching criteria = length . filter criteria . fmap (digits 10)

increasing :: [Int] -> Bool
increasing = go True
  where
    go acc [x]        = acc
    go acc (x:y:rest) = go (acc && x <= y) (y:rest)

part2 :: Solution In Out
part2 (min, max) = findMatching criteria [min..max]
  where
    criteria xs = increasing xs && any ((== 2) . length) (group xs)

problem :: Problem In Out
problem =
  Problem "src/day04.txt" parser part1Examples part1 part2Examples part2
  where
    part1Examples = []

    part2Examples = []
