module Day01 where

import Protolude
import Text.Megaparsec
import Text.Megaparsec.Text
import Prelude (read, last)

data Instruction
  = Instruction TurnDirection Int
  deriving (Show)

data TurnDirection
  = L
  | R
  deriving (Show)

data Position
  = Position Orientation (Int, Int)
  deriving (Show)

data Orientation
  = N
  | E
  | S
  | W
  deriving (Show)

start :: Position
start = Position N (0, 0)

manhattanDistance :: (Int, Int) -> Int
manhattanDistance (x, y) = abs x + abs y

followInstruction :: Position -> Instruction -> [Position]
followInstruction (Position o (x, y)) (Instruction d n) = Position o' <$> path
  where
    o' = turn d o
    Just path = tailMay $ case o' of
      N -> [(x, y') | y' <- [y..y + n]]
      W -> [(x', y) | x' <- [x, x - 1..x - n]]
      S -> [(x, y') | y' <- [y, y - 1..y - n]]
      E -> [(x', y) | x' <- [x..x + n]]

followInstructions :: [Instruction] -> [Position]
followInstructions is = concat ps
  where
    ps = [start] : zipWith continue ps is
    continue path = followInstruction (last path)

firstDup :: Eq a => [a] -> a
firstDup (a:as)
  | a `elem` as = a
  | otherwise = firstDup as

turn :: TurnDirection -> Orientation -> Orientation
turn L = \case
  N -> W
  W -> S
  S -> E
  E -> N
turn R = \case
  N -> E
  E -> S
  S -> W
  W -> N

parseDay01 :: Text -> [Instruction]
parseDay01 t = is
  where
    Right is = parse (instructionsP <* eof) "" t
    instructionsP :: Parser [Instruction]
    instructionsP = instructionP `sepBy` (char ',' *> char ' ')
    instructionP = Instruction <$> turnDirectionP <*> amountP
    turnDirectionP = (char 'R' *> pure R) <|> (char 'L' *> pure L)
    amountP = read <$> some digitChar