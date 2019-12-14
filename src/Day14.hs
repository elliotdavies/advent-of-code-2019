module Day14 where
  -- ( problem
  -- ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text       as Text
import           Prelude
import           Problem

type Chemical = Text.Text

type In = Map.Map Chemical (Int, [(Int, Chemical)])
type Out = Int

parser :: Parser In
parser = foldl toMap Map.empty . fmap parseLine . Text.lines
  where
    parseLine line =
      let [inputs, output] = Text.splitOn " => " line
       in (parseChemical output, parseChemical <$> (Text.splitOn ", " inputs))

    parseChemical s = let [n,chem] = Text.splitOn " " s in (read $ Text.unpack n, chem)

    toMap acc ((n, chem), deps) = Map.insert chem (n, deps) acc

type Spares = Map.Map Chemical Int

data State =
  State
    Int    -- Ore count
    Spares -- Leftover chemicals

part1 :: Solution In Out
part1 reactions = let (State count _) = generateFuel reactions (State 0 Map.empty) in count

generateFuel :: In -> State -> State
generateFuel reactions state = go state (1, "FUEL")
  where
    go :: State -> (Int, Chemical) -> State
    go (State count spares) (needed, chem) =
      let (needed', spares') = checkSpares needed chem spares
       in if needed' == 0
            then State count spares'
            else let (required, leftover) = requirementsFor needed' chem
                     spares'' = Map.insertWith (+) chem leftover spares'
                  in case required of
                      [(n, "ORE")] -> State (count + n) spares''
                      rest         -> foldl go (State count spares'') rest

    -- Use previously leftover chemicals if possible
    checkSpares :: Int -> Chemical -> Spares -> (Int, Spares)
    checkSpares n chem spare =
      case Map.lookup chem spare of
        Nothing -> (n, spare)
        Just x  -> if x >= n  then (0,     Map.insert chem (x - n) spare)
                              else (n - x, Map.delete chem spare)

    -- Run a reaction as many times as needed to generate the amount we want
    requirementsFor :: Int -> Chemical -> ([(Int, Chemical)], Int)
    requirementsFor goal chem =
      case Map.lookup chem reactions of
        Nothing       -> ([(goal,chem)], 0) -- Ore
        Just (n, req) -> if n >= goal then (req, n - goal)
                                      else let (req', leftover) = requirementsFor (goal - n) chem
                                            in (req ++ req', leftover)

part2 :: Solution In Out
part2 reactions =
  let (State ore spares) = generateFuel reactions (State 0 Map.empty)
      (iterations, oreRemaining) = trillion `quotRem` ore
    in iterations + go oreRemaining (Map.map (*iterations) spares)
  where
    go oreRemaining spares =
      let (State oreUsed spares') = generateFuel reactions (State 0 spares)
       in if oreUsed > oreRemaining then 0
                                    else 1 + go (oreRemaining - oreUsed) spares'

    trillion = 1000000000000

problem :: Problem In Out
problem =
  Problem "src/day14.txt" parser part1Examples part1 part2Examples part2
  where
    ex1 = Text.unlines
        [ "10 ORE => 10 A"
        , "1 ORE => 1 B"
        , "7 A, 1 B => 1 C"
        , "7 A, 1 C => 1 D"
        , "7 A, 1 D => 1 E"
        , "7 A, 1 E => 1 FUEL"
        ]

    ex2 = Text.unlines
        [ "9 ORE => 2 A"
        , "8 ORE => 3 B"
        , "7 ORE => 5 C"
        , "3 A, 4 B => 1 AB"
        , "5 B, 7 C => 1 BC"
        , "4 C, 1 A => 1 CA"
        , "2 AB, 3 BC, 4 CA => 1 FUEL"
        ]

    ex3 = Text.unlines
        [ "157 ORE => 5 NZVS"
        , "165 ORE => 6 DCFZ"
        , "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL"
        , "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ"
        , "179 ORE => 7 PSHF"
        , "177 ORE => 5 HKGWZ"
        , "7 DCFZ, 7 PSHF => 2 XJWVT"
        , "165 ORE => 2 GPVTF"
        , "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"
        ]

    ex4 = Text.unlines
        [ "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG"
        , "17 NVRVD, 3 JNWZP => 8 VPVL"
        , "53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL"
        , "22 VJHF, 37 MNCFX => 5 FWMGM"
        , "139 ORE => 4 NVRVD"
        , "144 ORE => 7 JNWZP"
        , "5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC"
        , "5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV"
        , "145 ORE => 6 MNCFX"
        , "1 NVRVD => 8 CXFTF"
        , "1 VJHF, 6 MNCFX => 4 RFSQX"
        , "176 ORE => 6 VJHF"
        ]

    ex5 = Text.unlines
        [ "171 ORE => 8 CNZTR"
        , "7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL"
        , "114 ORE => 4 BHXH"
        , "14 VRPVC => 6 BMBT"
        , "6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL"
        , "6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT"
        , "15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW"
        , "13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW"
        , "5 BMBT => 4 WPTQ"
        , "189 ORE => 9 KTJDG"
        , "1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP"
        , "12 VRPVC, 27 CNZTR => 2 XDBXC"
        , "15 KTJDG, 12 BHXH => 5 XCVML"
        , "3 BHXH, 2 VRPVC => 7 MZWV"
        , "121 ORE => 7 VRPVC"
        , "7 XCVML => 6 RJRHP"
        , "5 BHXH, 4 VRPVC => 5 LTCX"
        ]

    part1Examples =
      [ (ex1, 31)
      , (ex2, 165)
      , (ex3, 13312)
      , (ex4, 180697)
      , (ex5, 2210736)
      ]

    part2Examples =
      [ (ex3, 82892753)
      , (ex4, 5586022)
      , (ex5, 460664)
      ]
