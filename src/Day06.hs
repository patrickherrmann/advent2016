module Day06 where

import Protolude hiding (head)
import Prelude (head)
import Data.List (transpose, group)
import qualified Data.Text as Text

decodeMessage1 :: [[Char]] -> [Char]
decodeMessage1 = map (head . maximumBy (comparing length) . group . sort)

decodeMessage2 :: [[Char]] -> [Char]
decodeMessage2 = map (head . minimumBy (comparing length) . group . sort)

parseDay06 :: Text -> [[Char]]
parseDay06 = transpose . map toS . Text.lines
