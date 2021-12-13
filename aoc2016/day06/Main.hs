module Main where

import qualified Data.List as DL

-- count the occurence of every char and return a list of (count, char) tuples
countChars :: String -> [(Int, Char)]
countChars s = map (\xs@(x : _) -> (length xs, x)) $ DL.group $ DL.sort s

-- as the name says
mostCommonChar :: String -> Char
mostCommonChar s = snd $ maximum $ countChars s

-- as the name says
leastCommonChar :: String -> Char
leastCommonChar s = snd $ minimum $ countChars s

-- the given examples
example :: IO ()
example = do
  let input = "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar"
  -- "easter"
  putStrLn ("Example 1: " ++ map mostCommonChar (DL.transpose (words input)))
  -- "advent"
  putStrLn ("Example 2: " ++ map leastCommonChar (DL.transpose (words input)))

-- prob 1
problem1 :: IO ()
problem1 = do
  input <- readFile "input/input.txt"
  putStrLn ("Problem 1: " ++ map mostCommonChar (DL.transpose (words input)))

-- prob 2
problem2 :: IO ()
problem2 = do
  input <- readFile "input/input.txt"
  putStrLn ("Problem 2: " ++ map leastCommonChar (DL.transpose (words input)))

-- solve all my problems
main :: IO ()
main = do
  problem1
  problem2
