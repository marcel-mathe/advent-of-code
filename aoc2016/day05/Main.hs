module Main where

import Data.Digest.Pure.MD5
import Data.List (isPrefixOf)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

-- a valid md5 starts with 5 zeros in the hexadecimal representation
validMD5 :: MD5Digest -> Bool
validMD5 d = "00000" `isPrefixOf` show d

-- a infinite list of valid md5s for a given input plus
-- an integer postfix
generateZeroMD5s :: String -> [String]
generateZeroMD5s prefix = [y | x <- [0..],
    let y = prefix ++ show x,
    validMD5 (generateMD5 y)]

-- generate a single md5sum
generateMD5 :: String -> MD5Digest
generateMD5 = md5 . TLE.encodeUtf8 . TL.pack

-- get the sixth hex char from a md5sum
takeSixth :: String -> Char
takeSixth h = show (generateMD5 h) !! 5

-- example, pw should be: "18f47a30"
example :: IO ()
example = do
    print ("Example: " ++ map takeSixth (take 8 (generateZeroMD5s "abc")))

main :: IO ()
main = do
    print ("Problem 1: " ++ map takeSixth (take 8 (generateZeroMD5s "abbhdwsy")))
