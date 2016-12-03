module Day02 where

import Protolude
import qualified Data.Text as Text

data Direction
  = U
  | D
  | L
  | R
  deriving (Show)

findCode :: (Direction -> Char -> Char) -> [[Direction]] -> [Char]
findCode move lines = digits
  where
    digits = zipWith (foldl' (flip move)) seeds lines
    seeds = '5' : digits

moveDirection1 :: Direction -> Char -> Char
moveDirection1 = \case
  U -> \case
    '1' -> '1'
    '2' -> '2'
    '3' -> '3'
    (ord -> n) -> chr (n - 3)
  D -> \case
    '7' -> '7'
    '8' -> '8'
    '9' -> '9'
    (ord -> n) -> chr (n + 3)
  L -> \case
    '1' -> '1'
    '4' -> '4'
    '7' -> '7'
    (ord -> n) -> chr (n - 1)
  R -> \case
    '3' -> '3'
    '6' -> '6'
    '9' -> '9'
    (ord -> n) -> chr (n + 1)

moveDirection2 :: Direction -> Char -> Char
moveDirection2 = \case
  U -> \case
    '3' -> '1'
    '6' -> '2'
    '7' -> '3'
    '8' -> '4'
    'A' -> '6'
    'B' -> '7'
    'C' -> '8'
    'D' -> 'B'
    c -> c
  D -> \case
    '1' -> '3'
    '2' -> '6'
    '3' -> '7'
    '4' -> '8'
    '6' -> 'A'
    '7' -> 'B'
    '8' -> 'C'
    'B' -> 'D'
    c -> c
  L -> \case
    '3' -> '2'
    '4' -> '3'
    '6' -> '5'
    '7' -> '6'
    '8' -> '7'
    '9' -> '8'
    'B' -> 'A'
    'C' -> 'B'
    c -> c
  R -> \case
    '2' -> '3'
    '3' -> '4'
    '5' -> '6'
    '6' -> '7'
    '7' -> '8'
    '8' -> '9'
    'A' -> 'B'
    'B' -> 'C'
    c -> c

parseDay02 :: Text -> [[Direction]]
parseDay02 = map (map parseDir . Text.unpack) . Text.lines
  where
    parseDir = \case
      'U' -> U
      'D' -> D
      'L' -> L
      'R' -> R