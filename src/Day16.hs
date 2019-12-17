module Day16
  ( problem
  ) where

import           Data.Char       (digitToInt)
import           Data.Digits     (digits, unDigits)
import qualified Data.Map.Strict as Map
import qualified Data.Text       as Text
import           Prelude
import           Problem

type In = [Int]
type Out = Int

parser :: Parser In
parser = fmap digitToInt . Text.unpack . Text.strip

part1 :: Solution In Out
part1 input = unDigits 10 . take 8 . head . drop 100 . iterate phase $ input
  where
    indices = [1 .. length input]

    pattern x = take (length input) $ drop 1 $ cycle $ foldMap (replicate x) [0,1,0,-1]

    -- Attempt memoisation
    patterns = foldl (\acc i -> Map.insert i (pattern i) acc) Map.empty indices

    phase xs = calcElement <$> indices
      where
        calcElement idx = onesDigit $ sum $ zipWith (*) xs (patterns Map.! idx)

onesDigit :: Int -> Int
onesDigit 0 = 0
onesDigit n = last . digits 10 $ abs n

part2 :: Solution In Out
part2 input =
  let offset = unDigits 10 $ take 7 input
      input' = drop offset $ concat $ replicate 10000 input
   in part1 input'

problem :: Problem In Out
problem =
  Problem "src/day16.txt" parser part1Examples part1 part2Examples part2
  where
    part1Examples =
      [ ("80871224585914546619083218645595", 24176176)
      , ("19617804207202209144916044189917", 73745418)
      , ("69317163492948606335995924319873", 52432133)
      ]

    part2Examples =
      [ ("03036732577212944063491565474664", 84462026)
      , ("02935109699940807407585447034323", 78725270)
      , ("03081770884921959731165446850517", 53553731)
      ]
