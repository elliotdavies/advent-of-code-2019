module Day13
  ( problem
  ) where

import qualified Data.Map.Strict as Map
import           Linear.V2       (V2(..))
import           Prelude
import           Problem
import           Utils.Coords    (Grid, origin)
import           Utils.Intcode   (Memory, Program, ProgramState (..), mkProgram,
                                  parseMemory, runProgram)

type In = Memory
type Out = Int

parser :: Parser In
parser = parseMemory

data NextOutput = X | Y Int | Tile Int Int

data Tile
  = Empty
  | Wall
  | Block
  | Paddle
  | Ball
  deriving (Enum, Eq)

tile :: Int -> Tile
tile n = [Empty ..] !! n

data State = State
  { grid       :: Grid Tile
  , nextOutput :: NextOutput
  }

runUntilHalt :: State -> Program -> State
runUntilHalt state@State{..} program =
  case runProgram program of
    Halted _                -> state
    Await continue          -> error "Not expecting input"
    Yield (output,program') ->
      case nextOutput of
        X        -> runUntilHalt (state { nextOutput = Y output }) program'
        Y x      -> runUntilHalt (state { nextOutput = Tile x output }) program'
        Tile x y -> runUntilHalt (state { grid = markTile x y output, nextOutput = X }) program'
  where
    markTile x y tileId = Map.insert (V2 x y) (tile tileId) grid

part1 :: Solution In Out
part1 = Map.size . Map.filter (==Block) . grid . runUntilHalt initialState . mkProgram []
  where
    initialState = State { grid = Map.empty, nextOutput = X }

part2 :: Solution In Out
part2 memory = 0

problem :: Problem In Out
problem =
  Problem "src/day13.txt" parser part1Examples part1 part2Examples part2
  where
    part1Examples = []

    part2Examples = []
