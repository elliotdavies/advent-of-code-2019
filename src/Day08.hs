module Day08
  ( problem
  ) where

import           Data.Char   (digitToInt)
import           Data.List   (sortOn)
import qualified Data.Text   as Text
import qualified Data.Vector as V
import           Prelude
import           Problem

newtype Row = Row { unRow :: [Int] }
  deriving (Show)

newtype Layer = Layer { unLayer :: [Row] }
  deriving (Show)

newtype Image = Image { unImage :: [Layer] }
  deriving (Show)

type In = Image
type Out = Int

parser :: Parser In
parser input =
  let (size:image:_) = Text.lines input
      [width,height] = fmap (read . Text.unpack) $ Text.splitOn "," size
   in Image $ toLayers height $ toRows width $ Text.unpack image
  where
    chunksOf n xs =
      case splitAt n xs of
        (c, []) -> [c]
        (c,xs') -> c : chunksOf n xs'

    toRows size = fmap (Row . fmap digitToInt) . chunksOf size

    toLayers size = fmap Layer . chunksOf size

part1 :: Solution In Out
part1 (Image layers) =
  let layer = head $ sortOn (num 0) layers
   in num 1 layer * num 2 layer
  where
    num :: Int -> Layer -> Int
    num n = sum . fmap (length . filter (==n) . unRow) . unLayer

part2 :: Solution In Out
part2 input = 0

problem :: Problem In Out
problem =
  Problem "src/day08.txt" parser part1Examples part1 part2Examples part2
  where
    part1Examples = [("3,2\n121456789012345608", 2)]

    part2Examples = []
