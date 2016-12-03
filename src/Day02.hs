module Day02 where

import Protolude
import qualified Data.Text as Text

data Direction
  = U
  | D
  | L
  | R
  deriving (Show)

findCode1 :: [[Direction]] -> [Int]
findCode1 lines = digits
  where
    digits = zipWith followDirections1 seeds lines
    seeds = 5 : digits

followDirections1 :: Int -> [Direction] -> Int
followDirections1 = foldl' (flip moveDirection1)

moveDirection1 :: Direction -> Int -> Int
moveDirection1 = \case
  U -> \case
    1 -> 1
    2 -> 2
    3 -> 3
    n -> n - 3
  D -> \case
    7 -> 7
    8 -> 8
    9 -> 9
    n -> n + 3
  L -> \case
    1 -> 1
    4 -> 4
    7 -> 7
    n -> n - 1
  R -> \case
    3 -> 3
    6 -> 6
    9 -> 9
    n -> n + 1

parseDay02 :: Text -> [[Direction]]
parseDay02 = map (map parseDir . Text.unpack) . Text.lines
  where
    parseDir = \case
      'U' -> U
      'D' -> D
      'L' -> L
      'R' -> R