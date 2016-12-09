module Day08 where

import Protolude hiding (try)
import Prelude (read)
import Text.Megaparsec
import Text.Megaparsec.Text
import Data.Array.MArray
import Data.Array.ST

type Grid s = STUArray s Coord Bool

type Coord = (Int, Int)

data Instruction
  = Rect Int Int
  | RotateRow Int Int
  | RotateCol Int Int
  deriving (Show)

runInstructions :: [Instruction] -> Int
runInstructions is = runST $ do
  g <- emptyGrid
  traverse (followInstruction g) is
  es <- getElems g
  return $ length (filter identity es)

emptyGrid :: ST s (Grid s)
emptyGrid = newArray ((0, 0), (49, 5)) False

followInstruction :: Grid s -> Instruction -> ST s ()
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