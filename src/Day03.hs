module Day03 where

import Protolude
import qualified Data.Text as Text
import Data.List.Split (chunksOf)

triangleCount :: [[Int]] -> Int
triangleCount = length . filter isValidTriangle

isValidTriangle :: [Int] -> Bool
isValidTriangle [a, b, c] =
  a + b > c &&
  a + c > b &&
  b + c > a

transformInput :: [[Int]] -> [[Int]]
transformInput = chunksOf 3 . concat . transpose

parseDay03 :: Text -> [[Int]]
parseDay03 = map (map read . Text.words) . Text.lines
  where
    read t = let Just i = readMaybe (toS t) in i