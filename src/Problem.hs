module Problem
  ( Problem(..)
  , Parser
  , Examples
  , Solution

  , run
  ) where

import           Data.Foldable (for_)
import           Data.Text     (Text)
import           Data.Text     as Text
import           Prelude
import           Rainbow       (Chunk, chunk, fore, green, putChunkLn, red)


type Parser i = Text -> i

type Examples o = [(Text, o)]

type Solution i o = i -> o

data Problem i o =
  Problem { pInputFile     :: Text
          , pParser        :: Parser i
          , pPart1Examples :: Examples o
          , pPart1         :: Solution i o
          , pPart2Examples :: Examples o
          , pPart2         :: Solution i o
          }


run :: (Eq o, Show o) => Problem i o -> IO ()
run problem = do
  let Problem {..} = problem
  input <- Text.pack <$> (readFile $ Text.unpack pInputFile)

  putStrLn "Part 1"
  runExamples pPart1 pParser pPart1Examples
  runInput pPart1 $ pParser input

  -- putStrLn "Part 2"
  -- runExamples pPart2 pParser pPart2Examples
  -- runInput pPart2 $ pParser input


runExamples :: (Eq o, Show o) => Solution i o -> Parser i -> Examples o -> IO ()
runExamples solve parse examples = do
  putStrLn "Running tests:"
  for_ examples $ \(input, output) -> do
    let res = solve $ parse input

        msg = if res == output
                 then fore green "pass"
                 else fore red "fail"

    putChunkLn $ toChunk res <> " == " <> toChunk output <> ": " <> msg
  where
    toChunk = chunk . Text.pack . show


runInput :: Show o => Solution i o -> i -> IO ()
runInput solve input = do
  putStrLn "Running input:"
  putStrLn $ show $ solve input
