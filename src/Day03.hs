module Day03
  ( problem
  ) where

import qualified Data.Map.Strict as Map
import           Data.Text       (Text)
import qualified Data.Text       as Text
import           Linear.V2       (V2 (..))
import           Prelude
import           Problem
import           Utils.Coords    (Coords)
import qualified Utils.Coords    as Coords
import           Utils.Move      (Move, Walk(..), Path)
import qualified Utils.Move      as Move

type In = (Path, Path)
type Out = Int

parser :: Parser In
parser input =
  let p1:p2:_ = fmap parsePath $ Text.lines input
   in (p1, p2)
  where
    parsePath = fmap (parseMove . Text.unpack) . Text.splitOn ","

    parseMove (m:n) = (Move.readMove m, read n)

findCrossovers :: In -> Map.Map Coords Int
findCrossovers (p1,p2) =
  let Walk _ _ seen1 = Move.walkPath p1
      Walk _ _ seen2 = Move.walkPath p2
   in Map.intersectionWith (+) seen1 seen2

part1 :: Solution In Out
part1 = minimum . fmap (Coords.manhattan Coords.origin . fst) . Map.toList . findCrossovers

part2 :: Solution In Out
part2 = minimum . fmap snd . Map.toList . findCrossovers

problem :: Problem In Out
problem =
  Problem "src/day03.txt" parser part1Examples part1 part2Examples part2
  where
    examples
      = [ ("R8,U5,L5,D3\nU7,R6,D4,L4",)
        , ("R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83",)
        , ("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7",)
        ]

    part1Examples = zipWith ($) examples [6,159,135]

    part2Examples = zipWith ($) examples [30,610,410]
