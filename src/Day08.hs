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

data Image = Image { layers :: [Layer], size :: (Int, Int) }
  deriving (Show)

type In = Image
type Out = String

parser :: Parser In
parser input =
  let (size:image:_) = Text.lines input
      [width,height] = fmap (read . Text.unpack) $ Text.splitOn "," size
   in Image (toLayers height $ toRows width $ Text.unpack image) (width,height)
  where
    toRows size = fmap (Row . fmap digitToInt) . chunksOf size

    toLayers size = fmap Layer . chunksOf size

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs =
  case splitAt n xs of
    (c, []) -> [c]
    (c,xs') -> c : chunksOf n xs'

part1 :: Solution In Out
part1 (Image layers _) =
  let layer = head $ sortOn (num 0) layers
   in show $ num 1 layer * num 2 layer
  where
    num :: Int -> Layer -> Int
    num n = sum . fmap (length . filter (==n) . unRow) . unLayer

part2 :: Solution In Out
part2 (Image layers (width,height)) =
  let coords = [(w,h) | h <- [0.. height-1], w <- [0.. width-1]]
   in unlines $ fmap print $ chunksOf width $ map (uncurry stack) coords
  where
    stack w h = topmost $ fmap ((!! h) . fmap ((!! w) . unRow) . unLayer) layers

    topmost xs = case filter (/= 2) xs of
                   [] -> 2
                   (x:_) -> x

    print = concat . map (\case 0 -> " "; _ -> "#")

problem :: Problem In Out
problem =
  Problem "src/day08.txt" parser part1Examples part1 part2Examples part2
  where
    part1Examples = [("3,2\n121456789012345608", "2")]

    part2Examples = [("2,2\n0222112222120000", " #\n# \n")]
