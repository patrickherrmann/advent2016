module Day07 where

import Protolude hiding (try)
import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Data.Text as Text
import Data.List (nub)

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

supportsSsl :: IpAddress -> Bool
supportsSsl ip = any ((`elem` hypernetsAbas) . invertAba) supernetAbas
  where
    hypernetsAbas = nub $ ip >>= \case
      SupernetSequence _ -> []
      HypernetSequence t -> findAbas t
    supernetAbas = nub $ ip >>= \case
      SupernetSequence t -> findAbas t
      HypernetSequence _ -> []

findAbas :: Text -> [[Char]]
findAbas = filter isAba . map (take 3 . toS) . Text.tails
  where
    isAba (a:b:c:[]) = a == c && a /= b
    isAba _ = False

invertAba :: [Char] -> [Char]
invertAba (a:b:_) = [b, a, b]

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