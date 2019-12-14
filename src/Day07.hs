module Day07
  ( problem
  ) where

import           Control.Monad   (guard)
import           Data.List       (group, sort)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromJust)
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

  pure $ foldr go 0 phases
  where
    go phase input = fst $ fromJust $ runUntilOutput [phase,input] program

    phaseSettings = [0..4]

runUntilOutput :: [Int] -> Program -> Maybe (Int, Program)
runUntilOutput inputs program =
  case runProgram program of
        Halted _       -> Nothing
        Await continue -> runUntilOutput (tail inputs) (continue $ head inputs)
        Yield output   -> Just output

initialise :: Program -> Int -> Program
initialise program input =
  case runProgram program of
    Halted _       -> error "Unexpected halt"
    Await continue -> continue input
    Yield _        -> error "Unexpected output"

part2 :: Solution In Out
part2 program = maximum $ do
  p1 <- phaseSettings
  p2 <- phaseSettings
  p3 <- phaseSettings
  p4 <- phaseSettings
  p5 <- phaseSettings

  let phases = [p1,p2,p3,p4,p5]
  guard $ unique phases

  let programs = fmap (initialise program) phases

  pure $ go 0 programs
  where
    go input (p:ps) =
      case runUntilOutput [input] p of
        Just (output, p') -> go output (ps ++ [p'])
        Nothing           -> input

    phaseSettings = [5..9]

problem :: Problem In Out
problem =
  Problem "src/day07.txt" parser part1Examples part1 part2Examples part2
  where
    part1Examples =
      [ ("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0", 43210)
      , ("3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0", 54321)
      , ("3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0", 65210)
      ]

    part2Examples =
      [ ("3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5", 139629729)
      , ("3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10", 18216)
      ]
