module Day05 where

import Protolude
import Prelude ((!!))
import Crypto.Hash.MD5
import qualified Data.ByteString as BS
import Data.ByteString.Base16

constructPassword :: [(Int, Char)] -> [Char]
constructPassword ps = fill <$> [0..7]
  where
    fill i = let Just (_, c) = find ((== i) . fst) ps in c

getIndexAndChar :: ByteString -> (Int, Char)
getIndexAndChar (toS -> b) = (pos, b !! 6)
  where
    pos = ord (b !! 5) - ord '0'

getPasswordChar :: ByteString -> Char
getPasswordChar b = toS b !! 5

interestingHashes :: ByteString -> [ByteString]
interestingHashes code = filter isInteresting hashes
  where
    hashes = createHash <$> [(0 :: Int)..]
    createHash = encode . hash . (code <>) . show
    isInteresting = ("00000" `BS.isPrefixOf`)

parseDay05 :: Text -> ByteString
parseDay05 = toS