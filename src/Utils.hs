module Utils where

import           Debug.Trace (trace)
import           Prelude


debug :: Show a => String -> a -> a
debug s x = trace (s ++ " " ++ show x) x


chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs =
  case splitAt n xs of
    (c, []) -> [c]
    (c,xs') -> c : chunksOf n xs'
