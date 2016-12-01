module Day01 where

import Protolude
import qualified Data.Text as Text
import Data.Char

reverseCaps :: Text -> Text
reverseCaps = Text.reverse . Text.map toUpper

addBang :: Text -> Text
addBang = (<> "!")