{-| Find a string (containing of 'input' + a number 'n') whose MD5 hash will
a) start with 5 zeros
b) start with 6 zeros
-}
module Day4 where

import Crypto.Hash.MD5 as MD5 ( hash )
import Data.ByteString.Char8 as BS ( ByteString, take, pack, length )
import Data.ByteString.Base16 as B16 ( encode )
import Puzzle (Puzzle)

part1 :: Puzzle
part1 = show . findHash (BS.pack "00000") . BS.pack

part2 :: Puzzle
part2 = show . findHash (BS.pack "000000") . BS.pack

findHash :: ByteString -> ByteString -> Int
findHash match str = go 1 where
    go n = let h = B16.encode $ MD5.hash (str <> (BS.pack . show) n)
           in if matches h
               then n
               else go (n + 1)
    matches = (== match) . BS.take (fromIntegral l)
    l = BS.length match
