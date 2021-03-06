module Day04 where

import Protolude hiding (try)
import Prelude (read)
import qualified Data.Text as Text
import Text.Megaparsec
import Text.Megaparsec.Text

data Room = Room
  { roomName :: [Text]
  , sectorId :: Int
  , checksum :: Text
  } deriving (Show)

decryptRoom :: Room -> Room
decryptRoom r = r { roomName = name' }
  where
    name' = Text.map shiftChar <$> roomName r
    a = ord 'a'
    shiftChar c = chr $ (ord c - a + sectorId r) `rem` 26 + a

sectorIdSum :: [Room] -> Int
sectorIdSum = sum . map sectorId . filter passesChecksum

passesChecksum :: Room -> Bool
passesChecksum r = computeChecksum r == checksum r

computeChecksum :: Room -> Text
computeChecksum r = toS . take 5 $ sortedFreqs
  where
    sortedFreqs = map fst $ sortBy (comparing (Down . snd) <> comparing fst) freqs
    freqs = map (\cs@(c:_) -> (c, length cs)) . group . sort $ chars
    chars :: [Char]
    chars = concat . map toS $ roomName r

parseDay04 :: Text -> [Room]
parseDay04 t = r
  where
    Right r = parse roomsP "" t
    roomsP :: Parser [Room]
    roomsP = roomP `sepEndBy` eol
    roomP = do
      n <- encryptedNameP
      char '-'
      s <- sectorIdP
      c <- checksumP
      return $ Room (map toS n) s (toS c)
    encryptedNameP = do
      a <- some lowerChar
      bs <- many (try (char '-' *> some lowerChar))
      return $ a : bs
    sectorIdP = read <$> some digitChar
    checksumP = char '[' *> some lowerChar <* char ']'