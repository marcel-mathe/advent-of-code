module Main where

import Data.Char (isOctDigit)
import Data.Digest.Pure.MD5
import Data.List (isPrefixOf, nubBy, sort)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

-- a valid md5 starts with 5 zeros in the hexadecimal representation
validMD5 :: MD5Digest -> Bool
validMD5 d = "00000" `isPrefixOf` show d

-- a infinite list of valid md5s for a given input plus
-- an integer postfix
generateZeroMD5s :: String -> [String]
generateZeroMD5s prefix =
  [y | x <- [0 ..], let y = prefix ++ show x, validMD5 (generateMD5 y)]

-- generate a single md5sum
generateMD5 :: String -> MD5Digest
generateMD5 = md5 . TLE.encodeUtf8 . TL.pack

-- get the sixth hex char from a md5sum
takeSixth :: String -> Char
takeSixth h = show (generateMD5 h) !! 5

-- readability type
type Position = Char

-- get the sixth and sevent hex char
takeTuple :: String -> (Position, Char)
takeTuple h = (x, y)
  where
    hash = show (generateMD5 h)
    x = hash !! 5
    y = hash !! 6

-- a valid tuple has a Position between 0 and 7
validTuple :: (Position, Char) -> Bool
validTuple (p, _) = isOctDigit p

-- remove "duplicate" tuples, same first, different snd
nubby :: [(Position, Char)] -> [(Position, Char)]
nubby = nubBy (\x y -> fst x == fst y)

-- build the password string
generatePassword :: String -> String
generatePassword prefix =
  map snd $ sort $ take 8 $ nubby $ filter validTuple $ map takeTuple (generateZeroMD5s prefix)

-- examples
example :: IO ()
example = do
  -- "18f47a30"
  print ("Example 1: " ++ map takeSixth (take 8 (generateZeroMD5s "abc")))
  -- "05ace8e3"
  print ("Example 2: " ++ generatePassword "abc")

-- solve all my problems
main :: IO ()
main = do
  -- "801b56a7"
  print ("Problem 1: " ++ map takeSixth (take 8 (generateZeroMD5s "abbhdwsy")))
  -- "424a0197"
  print ("Problem 2: " ++ generatePassword "abbhdwsy")
