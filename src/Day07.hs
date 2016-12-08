module Day07 where

import Protolude hiding (try)
import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Data.Text as Text

data IpSequence
  = SupernetSequence Text
  | HypernetSequence Text
  deriving (Show)

type IpAddress = [IpSequence]

supportsTls :: IpAddress -> Bool
supportsTls ip = not (hasHypernetAbba ip) && hasSupernetAbba ip
  where
    hasHypernetAbba = any $ \case
      SupernetSequence _ -> False
      HypernetSequence t -> containsAbba t
    hasSupernetAbba = any $ \case
      SupernetSequence t -> containsAbba t
      HypernetSequence _ -> False

containsAbba :: Text -> Bool
containsAbba = any (isAbba . toS) . Text.tails
  where
    isAbba :: [Char] -> Bool
    isAbba (a:b:c:d:_) = a == d && b == c && a /= b
    isAbba _ = False

parseDay07 :: Text -> [IpAddress]
parseDay07 t = ips
  where
    Right ips = parse ipAddressesP "" t
    ipAddressesP :: Parser [IpAddress]
    ipAddressesP = ipAddressP `sepBy` eol
    ipAddressP = some ipSequenceP
    ipSequenceP = hypernetSequenceP <|> supernetSequenceP
    hypernetSequenceP = do
      try $ char '['
      cs <- some lowerChar
      char ']'
      return $ HypernetSequence (toS cs)
    supernetSequenceP = do
      cs <- some lowerChar
      return $ SupernetSequence (toS cs)