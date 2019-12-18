module Day11
  ( problem
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text       as Text
import           Linear.V2       (V2(..))
import           Prelude         hiding (print)
import           Problem
import           Utils           (chunksOf, debug)
import           Utils.Coords    (Coords, Grid, origin, getX, getY)
import           Utils.Intcode   (Memory, Program, ProgramState (..), mkProgram,
                                  parseMemory, runProgram)
import           Utils.Move      (Move(..), move)

type In = Memory
type Out = String

parser :: Parser In
parser = parseMemory

data OutputType = Color | Direction

data State = State
  { grid       :: Grid Int
  , coords     :: Coords
  , direction  :: Move
  , outputType :: OutputType
  }

runUntilHalt :: State -> Program -> State
runUntilHalt state@State{..} program =
  case runProgram program of
    Halted _                -> state
    Await continue          -> runUntilHalt state $ continue currentColour
    Yield (output,program') ->
      case outputType of
        Color     -> runUntilHalt (state { grid = markColour output, outputType = Direction}) program'
        Direction ->
          let direction' = turn output direction
           in runUntilHalt (state { coords = move direction' coords
                                  , direction = direction'
                                  , outputType = Color
                                  }) program'
  where
    currentColour = Map.findWithDefault 0 coords grid

    markColour colour = Map.insert coords colour grid

-- 0 is left, 1 is right
turn :: Int -> Move -> Move
turn 0 U = L
turn 0 L = D
turn 0 D = R
turn 0 R = U
turn 1 U = R
turn 1 L = U
turn 1 D = L
turn 1 R = D

part1 :: Solution In Out
part1 = show . Map.size . grid . runUntilHalt initialState . mkProgram []
  where
    initialState =
      State
      { grid       = Map.empty
      , coords     = origin
      , direction  = U
      , outputType = Color
      }

part2 :: Solution In Out
part2 = printGrid . grid . runUntilHalt initialState . mkProgram []
  where
    initialState =
      State
      { grid       = Map.singleton origin 1
      , coords     = origin
      , direction  = U
      , outputType = Color
      }

    printGrid grid =
      let keys = Map.keys grid
          maxX = maximum $ getX <$> keys
          minX = minimum $ getX <$> keys
          maxY = maximum $ getY <$> keys
          minY = minimum $ getY <$> keys
          -- Reverse because in this case the y coords are all negative so the
          -- image appears upside down
          coords = [V2 x y | y <- reverse [minY..maxY], x <- [minX..maxX]]
       in unlines $ fmap printLine $ chunksOf (maxX - minX) $ coords
      where
        -- @TODO Refactor grid printing and share with day 08
        printLine = concat . fmap print

        print cs = case Map.findWithDefault 0 cs grid of
                     0 -> " "
                     1 -> "#"

problem :: Problem In Out
problem =
  Problem "src/day11.txt" parser part1Examples part1 part2Examples part2
  where
    part1Examples = []

    part2Examples = []
