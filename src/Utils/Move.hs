module Utils.Move
  ( Move(..)
  , readMove
  , move

  , Path
  , Walk(..)
  , walkPath
  ) where

import qualified Data.Map.Strict as Map
import           Linear.V2       (V2 (..))
import           Prelude
import           Utils.Coords    (Coords, origin)


data Move
  = U
  | D
  | L
  | R

readMove :: Char -> Move
readMove = \case
  'U' -> U
  'D' -> D
  'L' -> L
  'R' -> R
  x   -> error $ "Unrecognised move: " ++ [x]

move :: Move -> Coords -> Coords
move m c = c + case m of
  U -> V2   0    1
  D -> V2   0  (-1)
  L -> V2 (-1)   0
  R -> V2   1    0

type Path = [(Move, Int)]

data Walk = Walk
      Coords               -- Current position
      Int                  -- Current number of steps taken
      (Map.Map Coords Int) -- Coords visited and steps taken to get there

walkPath :: Path -> Walk
walkPath = foldl walkMoves (Walk origin 0 Map.empty)
  where
    walkMoves state (m, n) = foldl (walkMove m) state [1..n]

    walkMove m (Walk cs steps seen) _ =
      let cs' = move m cs
          steps' = steps + 1
       in Walk cs' steps' (Map.insert cs' steps' seen)
