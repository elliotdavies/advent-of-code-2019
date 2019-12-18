module Day11
  ( problem
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text       as Text
import           Prelude
import           Problem
import           Utils.Coords    (Coords, Grid, origin)
import           Utils.Intcode   (Memory, Program, ProgramState (..), mkProgram,
                                  parseMemory, runProgram)
import           Utils.Move      (Move(..), move)

type In = Memory
type Out = Int

parser :: Parser In
parser = parseMemory

part1 :: Solution In Out
part1 = Map.size . grid . runUntilHalt initialState . mkProgram []
  where
    initialState =
      State
      { grid       = Map.empty
      , coords     = origin
      , direction  = U
      , outputType = Color
      }

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

part2 :: Solution In Out
part2 input = 0

problem :: Problem In Out
problem =
  Problem "src/day11.txt" parser part1Examples part1 part2Examples part2
  where
    part1Examples = []

    part2Examples = []
