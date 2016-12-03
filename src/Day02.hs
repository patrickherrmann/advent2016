module Day02 where

import Protolude
import qualified Data.Text as Text

data Direction
  = U
  | D
  | L
  | R
  deriving (Show)

findCode :: [[Direction]] -> [Int]
findCode lines = digits
  where
    digits = zipWith followDirections seeds lines
    seeds = 5 : digits

followDirections :: Int -> [Direction] -> Int
followDirections = foldl' (flip moveDirection)

moveDirection :: Direction -> Int -> Int
moveDirection = \case
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