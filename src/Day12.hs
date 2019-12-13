module Day12 where

import           Data.Functor    ((<&>))
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Data.Text       as Text
import           Linear.V3       (V3 (..))
import           Prelude
import           Problem

data Moon = Moon
  { position :: V3 Int
  , velocity :: V3 Int
  }
  deriving (Show, Ord, Eq)

type In = (Int, [Moon])
type Out = Int

parser :: Parser In
parser input =
  let (steps:positions) = Text.lines input
   in (read $ Text.unpack steps, parseMoon <$> positions)
  where
    parseMoon :: Text.Text -> Moon
    parseMoon line =
      let [x,y,z] =
            fmap Text.unpack $
            Text.splitOn "," $
            Text.filter (\c -> not $ any (c==) ['x','y','z','<','>','=',' ']) line

      in Moon { position = V3 (read x) (read y) (read z), velocity = V3 0 0 0 }

part1 :: Solution In Out
part1 (steps,moons) = totalEnergy $ last $ take (steps + 1) $ iterate step moons

step :: [Moon] -> [Moon]
step ms = ms <&> \m@(Moon{position = p, velocity = v}) ->
  let v' = v + sum (fmap (diff p . position) ms)
   in m { position = p + v', velocity = v' }

  where
    diff a b = ((.) signum . subtract) <$> a <*> b

totalEnergy :: [Moon] -> Int
totalEnergy = sum . map (\m -> sum (abs <$> position m) * sum (abs <$> velocity m))

part2 :: Solution In Out
part2 (_,moons) = go Set.empty moons
  where
    go seen ms
      = if Set.member ms seen
           then Set.size seen
           else go (Set.insert ms seen) (step ms)


problem :: Problem In Out
problem =
  Problem "src/day12.txt" parser part1Examples part1 part2Examples part2
  where
    ex1 = [ "<x=-1, y=0, z=2>"
          , "<x=2, y=-10, z=-7>"
          , "<x=4, y=-8, z=8>"
          , "<x=3, y=5, z=-1>"
          ]

    ex2 = [ "<x=-8, y=-10, z=0>"
          , "<x=5, y=5, z=10>"
          , "<x=2, y=-7, z=3>"
          , "<x=9, y=-8, z=-3>"
          ]

    part1Examples =
      [ (Text.unlines ("10":ex1), 179)
      , (Text.unlines ("100":ex2), 1940)
      ]

    part2Examples =
      [ (Text.unlines ("0":ex1), 2772)
      , (Text.unlines ("0":ex2), 4686774924)
      ]
