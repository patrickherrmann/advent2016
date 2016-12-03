module Day03 where

import Protolude
import qualified Data.Text as Text

isValidTriangle :: [Int] -> Bool
isValidTriangle [a, b, c] =
  a + b > c &&
  a + c > b &&
  b + c > a

parseDay03 :: Text -> [[Int]]
parseDay03 = map (map read . Text.words) . Text.lines
  where
    read t = let Just i = readMaybe (toS t) in i