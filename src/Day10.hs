module Day10
  ( problem
  , visible
  , parser
  ) where

import           Data.List       (sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Text       as Text
import           Linear.V2       (V2 (..))
import           Prelude
import           Problem
import           Utils.Coords    (Coords, Grid, manhattan)


data Position = Empty | Asteroid
  deriving (Eq, Show)

type Angle = V2 Int

type In = Grid Position
type Out = (Coords, Int)

parser :: Parser In
parser = foldl parseLine Map.empty . zip [0..] . Text.lines
  where
    parseLine grid (y,line) = foldl parsePoint grid $ zip [0..] $ Text.unpack line
      where
        parsePoint grid (x,point) =
          Map.insert (V2 x y) (if point == '#' then Asteroid else Empty) grid

part1 :: Solution In Out
part1 grid
  = head
  . reverse
  . sortOn snd
  . Map.assocs
  $ Map.mapWithKey (\cs _ -> length $ Map.elems $ visible asteroids cs) asteroids
  where
    asteroids = Map.filter (== Asteroid) grid

visible :: Grid Position -> Coords -> Map.Map Angle Coords
visible grid coords
  = Map.foldrWithKey  (\cs angle -> Map.insertWith nearer angle cs) Map.empty
  $ Map.mapWithKey    (\k _ -> angleFrom coords k)
  $ Map.filterWithKey (\k _ -> k /= coords) grid
    where
      nearer cs cs' = if manhattan coords cs < manhattan coords cs' then cs else cs'

angleFrom :: Coords -> Coords -> Angle
angleFrom v v' = let cs@(V2 x y) = v' - v in (`div` (gcd x y)) <$> cs

part2 :: Solution In Out
part2 input = (V2 0 0, 0)

problem :: Problem In Out
problem =
  Problem "src/day10.txt" parser part1Examples part1 part2Examples part2
  where
    part1Examples =
      [ (Text.unlines
        [ ".#..#"
        , "....."
        , "#####"
        , "....#"
        , "...##"
        ]
        , (V2 3 4, 8))

      , (Text.unlines
        [ "......#.#."
        , "#..#.#...."
        , "..#######."
        , ".#.#.###.."
        , ".#..#....."
        , "..#....#.#"
        , "#..#....#."
        , ".##.#..###"
        , "##...#..#."
        , ".#....####"
        ]
      , (V2 5 8, 33))

      , (Text.unlines
        [ "#.#...#.#."
        , ".###....#."
        , ".#....#..."
        , "##.#.#.#.#"
        , "....#.#.#."
        , ".##..###.#"
        , "..#...##.."
        , "..##....##"
        , "......#..."
        , ".####.###."
        ]
      , (V2 1 2, 35))

      , (Text.unlines
        [ ".#..#..###"
        , "####.###.#"
        , "....###.#."
        , "..###.##.#"
        , "##.##.#.#."
        , "....###..#"
        , "..#.#..#.#"
        , "#..#.#.###"
        , ".##...##.#"
        , ".....#.#.."
        ]
      , (V2 6 3, 41))

      , (Text.unlines
        [ ".#..##.###...#######"
        , "##.############..##."
        , ".#.######.########.#"
        , ".###.#######.####.#."
        , "#####.##.#.##.###.##"
        , "..#####..#.#########"
        , "####################"
        , "#.####....###.#.#.##"
        , "##.#################"
        , "#####.##.###..####.."
        , "..######..##.#######"
        , "####.##.####...##..#"
        , ".#####..#.######.###"
        , "##...#.##########..."
        , "#.##########.#######"
        , ".####.#.###.###.#.##"
        , "....##.##.###..#####"
        , ".#.#.###########.###"
        , "#.#.#.#####.####.###"
        , "###.##.####.##.#..##"
        ]
      , (V2 11 13, 210))
      ]

    part2Examples = []
