module Day06
  ( problem
  , distanceToLeaf
  ) where

import qualified Data.Map.Strict as Map
import           Data.Maybe      (catMaybes, fromJust, fromMaybe)
import           Data.Text       (Text)
import qualified Data.Text       as Text
import           Prelude
import           Problem

type In = Tree
type Out = Int

type Id = Text
type Tree = Map.Map Id [Id]

parser :: Parser In
parser = buildTree . fmap (toTuple . Text.splitOn ")") . Text.lines
  where
    toTuple [a,b] = (a,b)

    buildTree = foldr (\(node,child) acc -> Map.insertWith (++) node [child] acc) Map.empty


part1 :: Solution In Out
part1 tree = countOrbits "COM"
  where
    countOrbits node = countTree node + sum (countOrbits <$> childrenOf node tree)
      where
        countTree node =
          let cs = childrenOf node tree
           in length cs + sum (fmap countTree cs)


childrenOf :: Id -> Tree -> [Id]
childrenOf node = fromMaybe [] . Map.lookup node


part2 :: Solution In Out
part2 tree =
  let you = parentOf "YOU"
      san = parentOf "SAN"

      -- Start at the root and descend until we run out of common ancestors
   in fromJust $ go you san "COM" 
  where
    go a b root =
      case (distanceToLeaf tree a root, distanceToLeaf tree b root) of
        (Just x, Just y) ->
          Just $ minimum $ (x+y) : catMaybes (fmap (go a b) (childrenOf root tree))

        _ -> Nothing

    parentOf child = head . Map.keys $ Map.filter (elem child) tree


distanceToLeaf :: Tree -> Id -> Id -> Maybe Int
distanceToLeaf tree = go
  where
    go leaf root = Map.lookup root tree >>= \cs ->
      if leaf `elem` cs
        then Just 1
        else let options = catMaybes $ fmap (go leaf) cs
              in if null options then Nothing else Just $ 1 + minimum options


problem :: Problem In Out
problem =
  Problem "src/day06.txt" parser part1Examples part1 part2Examples part2
  where
    part1Examples = [(Text.unlines ["COM)B","B)C","C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L"], 42)]

    part2Examples = [(Text.unlines ["COM)B","B)C","C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L","K)YOU","I)SAN"], 4)]
