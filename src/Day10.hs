module Day10
  ( problem
  ) where

import           Data.List       (sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Text       as Text
import           Data.Tuple      (swap)
import           Linear.V2       (V2 (..))
import           Prelude
import           Problem
import           Utils.Coords    (Coords, manhattan)

data Position = Empty | Asteroid
  deriving (Eq, Show)

newtype Ratio = Ratio (V2 Int)
  deriving (Eq, Ord)

type In = [Coords]
type Out = Int

parser :: Parser In
parser = Map.keys . Map.filter (== Asteroid) . foldl parseLine Map.empty . zip [0..] . Text.lines
  where
    parseLine grid (y,line) = foldl parsePoint grid $ zip [0..] $ Text.unpack line
      where
        parsePoint grid (x,point) =
          Map.insert (V2 x y) (if point == '#' then Asteroid else Empty) grid


part1 :: Solution In Out
part1 = snd . findStation


findStation :: [Coords] -> (Coords, Int)
findStation asteroids
  = head $ reverse $ sortOn snd $ fmap (\cs -> (cs, length $ visible cs asteroids)) asteroids


visible :: Coords -> [Coords] -> [(Coords, Ratio)]
visible coords
  = fmap swap . Map.assocs
  . foldr (\(cs,ratio) -> Map.insertWith nearer ratio cs) Map.empty -- De-duplicate points along the same line-of-sight
  . fmap (\cs -> (cs, ratioFrom coords cs))
  . filter (/= coords)
    where
      nearer :: Coords -> Coords -> Coords
      nearer cs cs' = if manhattan coords cs < manhattan coords cs' then cs else cs'

      ratioFrom :: Coords -> Coords -> Ratio
      ratioFrom v v' = let cs@(V2 x y) = v' - v in Ratio $ (`div` (gcd x y)) <$> cs


part2 :: Solution In Out
part2 asteroids =
  let V2 x y = (sweep [] (filter (/= station) asteroids)) !! 199
   in x * 100 + y
  where
    station = fst $ findStation asteroids

    sweep :: [Coords] -> [Coords] -> [Coords]
    sweep destroyed remaining =
      case remaining of
        [] -> destroyed
        cs ->
          let detected = fmap fst $ reverse $ sortOn snd $ fmap (fmap angle) $ visible station cs
           in sweep (destroyed ++ detected) $ filter (\k -> not $ k `elem` detected) cs

    angle :: Ratio -> Double
    angle (Ratio (V2 x y)) = atan2 (fromIntegral x) (fromIntegral y)

problem :: Problem In Out
problem =
  Problem "src/day10.txt" parser part1Examples part1 part2Examples part2
  where
    ex = Text.unlines
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

    part1Examples =
      [ (Text.unlines
        [ ".#..#"
        , "....."
        , "#####"
        , "....#"
        , "...##"
        ]
        , 8)

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
      , 33)

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
      , 35)

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
      , 41)

      , (ex, 210)
      ]

    part2Examples =
      [ (ex, 802)
      ]
