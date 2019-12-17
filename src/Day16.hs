module Day16
  ( problem
  ) where

import           Data.Char       (digitToInt)
import           Data.Digits     (digits, unDigits)
import qualified Data.Text       as Text
import           Prelude
import           Problem

type In = [Int]
type Out = Int

parser :: Parser In
parser = fmap digitToInt . Text.unpack . Text.strip

part1 :: Solution In Out
part1 = unDigits 10 . take 8 . head . drop 100 . iterate phase

phase :: [Int] -> [Int]
phase input = element <$> [1 .. length input]
  where
    element idx = onesDigit $ sum $ zipWith (*) input (pattern idx)

    pattern x = drop 1 $ cycle $ foldMap (replicate x) [0,1,0,-1]

    onesDigit 0 = 0
    onesDigit n = last . digits 10 $ abs n

part2 :: Solution In Out
part2 input = 0

problem :: Problem In Out
problem =
  Problem "src/day16.txt" parser part1Examples part1 part2Examples part2
  where
    part1Examples =
      [ ("80871224585914546619083218645595", 24176176)
      , ("19617804207202209144916044189917", 73745418)
      , ("69317163492948606335995924319873", 52432133)
      ]

    part2Examples = []
