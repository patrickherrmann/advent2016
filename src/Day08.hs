module Day08 where

import Protolude hiding (try)
import Prelude (read)
import Text.Megaparsec
import Text.Megaparsec.Text
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed
import Data.List.Split (chunksOf)
import qualified Data.Text as Text

type MutableGrid s = STUArray s Coord Bool
type Grid = UArray Coord Bool

type Coord = (Int, Int)

data Instruction
  = Rect Int Int
  | RotateRow Int Int
  | RotateCol Int Int
  deriving (Show)

measureVoltage :: Grid -> Int
measureVoltage = length . filter identity . elems

displayGrid :: Grid -> Text
displayGrid g = Text.unlines $ map (toS . formatLine) lines
  where
    formatLine = intercalate " " . chunksOf 5
    lines = transpose $ chunksOf 6 pixels
    pixels = displayPixel <$> elems g
    displayPixel = \case
      True -> '#'
      False -> '.'

runInstructions :: [Instruction] -> Grid
runInstructions is = runSTUArray $ do
  g <- emptyMutableGrid
  traverse (followInstruction g) is
  return g

emptyMutableGrid :: ST s (MutableGrid s)
emptyMutableGrid = newArray ((0, 0), (49, 5)) False

followInstruction :: MutableGrid s -> Instruction -> ST s ()
followInstruction g = \case
  Rect x y -> do
    let coords = [(a, b) | a <- [0..x-1], b <- [0..y-1]]
    traverse_ (\c -> writeArray g c True) coords
  RotateRow r amount -> do
    let rowCoords = [(x, r) | x <- [0..49]]
    row <- traverse (readArray g) rowCoords
    let rowCoords' = drop amount (cycle rowCoords)
    let writes = zip rowCoords' row
    traverse_ (\(i, e) -> writeArray g i e) writes
  RotateCol c amount -> do
    let colCoords = [(c, y) | y <- [0..5]]
    col <- traverse (readArray g) colCoords
    let colCoords' = drop amount (cycle colCoords)
    let writes = zip colCoords' col
    traverse_ (\(i, e) -> writeArray g i e) writes

parseDay08 :: Text -> [Instruction]
parseDay08 t = is
  where
    Right is = parse instructionsP "" t
    instructionsP :: Parser [Instruction]
    instructionsP = instructionP `sepBy` eol
    instructionP = rectP <|> rotateRowP <|> rotateColP
    rectP = do
      try $ string "rect "
      x <- numP
      char 'x'
      y <- numP
      return $ Rect x y
    rotateRowP = do
      try $ string "rotate row y="
      r <- numP
      string " by "
      amount <- numP
      return $ RotateRow r amount
    rotateColP = do
      try $ string "rotate column x="
      c <- numP
      string " by "
      amount <- numP
      return $ RotateCol c amount
    numP = read <$> some digitChar