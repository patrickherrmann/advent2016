module Day05 where

import Protolude
import Prelude ((!!))
import Crypto.Hash.MD5
import qualified Data.ByteString as BS
import Data.ByteString.Base16

getPasswordChar :: ByteString -> Char
getPasswordChar b = toS b !! 5

interestingHashes :: ByteString -> [(Int, ByteString)]
interestingHashes code = filter (isInteresting . snd) hashes
  where
    hashes = createHash <$> [0..]
    createHash i = (i, encode . hash $ code <> show i)
    isInteresting = ("00000" `BS.isPrefixOf`)

parseDay05 :: Text -> ByteString
parseDay05 = toS