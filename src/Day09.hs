module Day09 where

import Protolude hiding (try)
import Prelude (read)
import Text.Megaparsec
import Text.Megaparsec.Text

decompress1 :: Text -> Text
decompress1 t = p
  where
    Right p = parse plaintextP "" t
    plaintextP :: Parser Text
    plaintextP = mconcat <$> some (try markerP <|> charP)
    charP = do
      c <- anyChar
      return $ toS [c]
    markerP = do
      (range, reps) <- do
        char '('
        range <- numP
        char 'x'
        reps <- numP
        char ')'
        return (range, reps)
      cs <- replicateM range anyChar
      let expanded = toS (concat (replicate reps cs))
      return expanded

decompress2 :: Text -> Int
decompress2 t = p
  where
    Right p = parse plaintextP "" t
    plaintextP :: Parser Int
    plaintextP = sum <$> some (try markerP <|> charP)
    charP = anyChar *> return 1
    markerP = do
      (range, reps) <- do
        char '('
        range <- numP
        char 'x'
        reps <- numP
        char ')'
        return (range, reps)
      cs <- replicateM range anyChar
      return $ reps * decompress2 (toS cs)

numP :: Parser Int
numP = read <$> some digitChar