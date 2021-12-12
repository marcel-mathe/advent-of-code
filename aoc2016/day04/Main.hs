module Main where

import System.IO
import Data.List (sortBy)
import Data.Function (on)
import qualified Data.Map as M

import Parser

-- count characters in a string,
-- return Map of Tuples (character, count)
countCharRate :: String -> M.Map Char Int
countCharRate = foldl f M.empty
    where f acc c = if M.member c acc
            then M.update (Just . succ) c acc
            else M.insert c 1 acc

-- sort by second entry of a tuple in a list
secondSort :: (Ord a, Ord b) => [(a, b)] -> [(a, b)]
secondSort = sortBy (flip compare `on` snd)

-- get keys from (key, value) tuple list
keys :: [(Char, Int)] -> String
keys [] = []
keys [(c, i)] = [c]
keys ((c, i):rest) = c : keys rest

-- createChecksum for a given String
checkSum :: String -> String
checkSum s = take 5 $ keys  $ secondSort $ M.toList $ countCharRate s

-- check for room validity
validRoom :: Room -> Bool
validRoom r = check r == checkSum (name r)

-- sum the sector numbers of all rooms
sumSector :: [Room] -> Int
sumSector = foldl (\acc r -> sector r + acc) 0

-- shift a character n times
shiftChar :: Int -> Char -> Char
shiftChar 0  c  = c
shiftChar n 'z' = shiftChar (pred n) 'a'
shiftChar n  c  = shiftChar (pred (n `mod` 26)) (succ c)

example :: IO ()
example = do
    let xs = [parseRoom "aaaaa-bbb-z-y-x-123[abxyz]"
            , parseRoom "a-b-c-d-e-f-g-h-987[abcde]"
            , parseRoom "not-a-real-room-404[oarel]"
            , parseRoom "totally-real-room-200[decoy]"]
    putStrLn $ "example: " ++ show (sumSector $ filter validRoom xs)

problem1 :: IO ()
problem1 = do
    content <- readFile "input/input.txt"
    putStrLn $ "Problem 1: " ++ show (sumSector $ filter validRoom $ parseRooms content)

problem2 :: IO ()
problem2 = do
    content <- readFile "input/input.txt"
    -- room -> (decoded name, sector)
    let f r = (map (shiftChar (sector r)) (name r), sector r)
    -- check for the correct name
    let g (name, sector) = "northpoleobjectstorage" == name
    -- find the good old north pole object storage
    let nos = snd $ head $ filter g $ map f $ filter validRoom (parseRooms content)
    -- output
    putStrLn $ "Problem2: " ++ show nos

main :: IO ()
main = do
    problem1
    problem2
