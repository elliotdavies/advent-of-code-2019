module Day07
  ( problem
  ) where

import           Control.Monad   (guard)
import           Data.List       (group, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Vector     as V
import           Prelude
import           Problem
import           Utils.Intcode   (Program, ProgramState (..), mkProgram,
                                  parseMemory, runProgram)

type In = Program
type Out = Int

parser :: Parser In
parser = mkProgram [] . parseMemory

unique :: [Int] -> Bool
unique = all ((==1) . length) . group . sort

part1 :: Solution In Out
part1 program = maximum $ do
  p1 <- phaseSettings
  p2 <- phaseSettings
  p3 <- phaseSettings
  p4 <- phaseSettings
  p5 <- phaseSettings

  let phases = [p1,p2,p3,p4,p5]
  guard $ unique phases
  pure $ foldr (\phase input -> go program [phase,input]) 0 phases
  where
    go :: Program -> [Int] -> Int
    go program inputs =
      case runProgram program of
        Halted _         -> error "Should have received output by now"
        Await continue   -> go (continue $ head inputs) (tail inputs)
        Yield (output,_) -> output

    phaseSettings = [0..4]

part2 :: Solution In Out
part2 program = 0

problem :: Problem In Out
problem =
  Problem "src/day07.txt" parser part1Examples part1 part2Examples part2
  where
    part1Examples =
      [ ("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0", 43210)
      , ("3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0", 54321)
      , ("3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0", 65210)
      ]

    part2Examples = []
