module Day13
  ( problem
  ) where

import qualified Data.Map.Strict as Map
import           Linear.V2       (V2(..))
import           Prelude
import           Problem
import           Utils.Coords    (Grid, origin, getX, getY)
import           Utils.Intcode   (Memory, Program, ProgramState (..), mkProgram,
                                  parseMemory, runProgram)

type In = Memory
type Out = Int

parser :: Parser In
parser = parseMemory

data NextOutput
  = X
  | Y Int
  | Tile Int Int
  | Score

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
  , score      :: Int
  }

runUntilHalt :: (Grid Tile -> Int) -> State -> Program -> State
runUntilHalt decideInput state@State{..} program =
  case runProgram program of
    Halted _                -> state
    Await continue          -> runUntilHalt decideInput state $ continue (decideInput grid)
    Yield (output,program') ->
      case nextOutput of
        X        -> runUntilHalt decideInput (state { nextOutput = Y output }) program'
        Y x      ->
          let nextOutput = case (x,output) of (-1, 0) -> Score; _ -> Tile x output
           in runUntilHalt decideInput (state { nextOutput }) program'
        Tile x y -> runUntilHalt decideInput (state { grid = markTile x y output, nextOutput = X }) program'
        Score    -> runUntilHalt decideInput (state { score = output, nextOutput = X }) program'
  where
    markTile x y tileId = Map.insert (V2 x y) (tile tileId) grid

part1 :: Solution In Out
part1 = Map.size . Map.filter (==Block) . grid . runUntilHalt (const 0) initialState . mkProgram []
  where
    initialState = State { grid = Map.empty, nextOutput = X, score = 0 }

part2 :: Solution In Out
part2 = score . runUntilHalt decideInput initialState . mkProgram [(0,2)]
  where
    initialState = State { grid = Map.empty, nextOutput = X, score = 0 }

    decideInput grid =
      let ballPos   = head $ Map.keys $ Map.filter (==Ball) grid
          paddlePos = head $ Map.keys $ Map.filter (==Paddle) grid
       in getX ballPos - getX paddlePos

problem :: Problem In Out
problem =
  Problem "src/day13.txt" parser part1Examples part1 part2Examples part2
  where
    part1Examples = []

    part2Examples = []
