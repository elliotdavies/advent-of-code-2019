module Utils.Coords
  ( Coords(..)
  , manhattan
  , origin
  , Grid(..)
  , getX
  , getY
  ) where

import qualified Data.Map.Strict as Map
import           Linear.V2       (V2 (..))
import           Prelude


type Coords = V2 Int

getX :: Coords -> Int
getX (V2 x _) = x

getY :: Coords -> Int
getY (V2 _ y) = y

origin :: Coords
origin = V2 0 0

manhattan :: Coords -> Coords -> Int
manhattan c1 c2 =
  let V2 x y = c1 - c2
   in abs x + abs y

type Grid a = Map.Map Coords a
