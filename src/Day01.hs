module Day01 where

import Protolude
import Text.Megaparsec
import Text.Megaparsec.Text

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

followInstructions :: [Instruction] -> Position
followInstructions = foldl' followInstruction start

followInstruction :: Position -> Instruction -> Position
followInstruction (Position o (x, y)) (Instruction d n) = Position o' l'
  where
    o' = turn d o
    l' = case o' of
      N -> (x, y + n)
      W -> (x - n, y)
      S -> (x, y - n)
      E -> (x + n, y)

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

parseInstructions :: Text -> [Instruction]
parseInstructions t = is
  where
    Right is = parse (instructions <* eof) "" t
    instructions :: Parser [Instruction]
    instructions = instruction `sepBy` (char ',' *> char ' ')
    instruction = Instruction <$> turnDirection <*> amount
    turnDirection = (char 'R' *> pure R) <|> (char 'L' *> pure L)
    amount = do
      ds <- some digitChar
      let Just a = readMaybe ds
      return a