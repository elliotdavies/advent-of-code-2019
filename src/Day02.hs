module Day02
  ( problem
  ) where

import qualified Data.Text   as Text
import qualified Data.Vector as V
import           Prelude
import           Problem

type In = V.Vector Int
type Out = V.Vector Int

parser :: Parser In
parser = V.fromList . fmap (read . Text.unpack) . Text.splitOn ","

data Program = Program Int (V.Vector Int)

part1 :: Solution In Out
part1 input =
  let initial = Program 0 (input V.// [(1, 12), (2, 2)]) -- TODO
      (Program _ finalState) = runProgram initial
    in finalState

runProgram :: Program -> Program
runProgram program@(Program idx state) =
  case value idx of
    1  -> runProgram $ Program (idx + 4) (operate (+))
    2  -> runProgram $ Program (idx + 4) (operate (*))
    99 -> program
    _  -> error $ "Unrecognised opcode: " ++ show idx
  where
    operate op =
      let res = reference (idx + 1) `op` reference (idx + 2)
          store = value (idx + 3)
      in  state V.// [(store, res)]

    reference i = state V.! (value i)

    value i = state V.! i

part2 :: Solution In Out
part2 input = V.empty

problem :: Problem In Out
problem =
  Problem "src/day02.txt" parser part1Examples part1 part2Examples part2
  where
    part1Examples =
      [
      -- [("1,9,10,3,2,3,11,0,99,30,40,50", V.fromList [3500,9,10,70,2,3,11,0,99,30,40,50]),
      --  ("1,0,0,0,99"                   , V.fromList [2,0,0,0,99]),
      --  ("2,3,0,3,99"                   , V.fromList [2,3,0,6,99]),
      --  ("2,4,4,5,99,0"                 , V.fromList [2,4,4,5,99,9801]),
      --  ("1,1,1,4,99,5,6,0,99"          , V.fromList [30,1,1,4,2,5,6,0,99])
       ]

    part2Examples = []
