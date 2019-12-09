module Utils where

import           Debug.Trace (trace)
import           Prelude


debug :: Show a => String -> a -> a
debug s x = trace (s ++ " " ++ show x) x
