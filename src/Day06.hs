module Day06 where

import Protolude hiding (head)
import Prelude (head)
import Data.List (transpose, group)
import qualified Data.Text as Text

decodeMessage :: [[Char]] -> [Char]
decodeMessage = map decodeCol
  where
    decodeCol = head . maximumBy (comparing length) . group . sort

parseDay06 :: Text -> [[Char]]
parseDay06 = transpose . map toS . Text.lines
