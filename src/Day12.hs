module Day12
  ( problem
  , parser
  , part1
  , gravity
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text       as Text
import           Linear.V3       (V3 (..))
import           Prelude
import           Problem

import           Debug.Trace     (traceShowId)

data Moon = Moon
  { position :: V3 Int
  , velocity :: V3 Int
  }
  deriving (Show)

type In = (Int, Map.Map Int Moon)
type Out = Int

parser :: Parser In
parser input =
  let (steps:positions) = Text.lines input
   in (read $ Text.unpack steps, Map.fromList $ zip [0..] $ parseMoon <$> positions)
  where
    parseMoon :: Text.Text -> Moon
    parseMoon line =
      let [x,y,z] =
            fmap Text.unpack $
            Text.splitOn "," $
            Text.filter (\c -> not $ any (c==) ['x','y','z','<','>','=',' ']) line

      in Moon { position = V3 (read x) (read y) (read z), velocity = V3 0 0 0 }

part1 :: Solution In Out
part1 (step,moons) =
  let pairs = [(a,b) | a <- [0.. Map.size moons - 1], b <- [0.. Map.size moons - 1], a /= b]
   in Map.size $ traceShowId $ foldl go moons pairs
  where
    go acc (a,b) =
      let (mA,mB) = gravity (acc Map.! a, acc Map.! b)
       in Map.insert a mA (Map.insert b mB acc)


gravity :: (Moon, Moon) -> (Moon, Moon)
gravity (m, m') =
  let v  = position m
      v' = position m'
    in ( m  { position = v + (signum <$> v - v') }
       , m' { position = v' + (signum <$> v' - v) }
       )

part2 :: Solution In Out
part2 moons = 0

problem :: Problem In Out
problem =
  Problem "src/day12.txt" parser part1Examples part1 part2Examples part2
  where
    part1Examples =
      [ (Text.unlines
        [ "10"
        , "<x=-1, y=0, z=2>"
        , "<x=2, y=-10, z=-7>"
        , "<x=4, y=-8, z=8>"
        , "<x=3, y=5, z=-1>"
        ], 179)
      , (Text.unlines
        [ "100"
        , "<x=-8, y=-10, z=0>"
        , "<x=5, y=5, z=10>"
        , "<x=2, y=-7, z=3>"
        , "<x=9, y=-8, z=-3>"
        ], 1940)
      ]

    part2Examples = []
