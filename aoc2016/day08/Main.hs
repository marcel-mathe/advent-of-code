module Main where

import Control.Lens
import Data.List
import Parser
import Types

-- pixel is on
on = '#'

-- pixel is off
off = '.'

-- convert rectangle commands into coord tuples
rectangleCoords :: Command -> [(Int, Int)]
rectangleCoords (Rect b a) = [(x, y) | x <- [0 .. (a - 1)], y <- [0 .. (b - 1)]]

-- update the value of the grid at position (x, y) to $char
updateLight :: Char -> (Int, Int) -> Grid -> Grid
updateLight char (x, y) grid = grid & ix x . ix y .~ char

-- execute rect command on a given matrix
rect :: Grid -> Command -> Grid
rect g c = foldr (updateLight on) g (rectangleCoords c)

-- rotate list right by one
rotateList :: [a] -> [a]
rotateList [] = []
rotateList [x] = [x]
rotateList xs = last xs : init xs

-- rotate right by n
-- rotateBy 2 [1,2,3] -> [2,3,1]
rotateBy :: Int -> [a] -> [a]
rotateBy n [] = []
rotateBy 0 xs = xs
rotateBy n xs = (!! (n `mod` length xs)) . iterate rotateList $ xs

-- rotate a given column in a grid
columnRotate :: Grid -> Command -> Grid
columnRotate g (ColumnRotate a b) = transpose $ transpose g & ix a %~ rotateBy b

-- rotate a given row in a grid
rowRotate :: Grid -> Command -> Grid
rowRotate g (RowRotate a b) = g & ix a %~ rotateBy b

-- command switch
execCommand :: Grid -> Command -> Grid
execCommand g c@(Rect _ _) = rect g c
execCommand g c@(RowRotate _ _) = rowRotate g c
execCommand g c@(ColumnRotate _ _) = columnRotate g c

-- create a grid of width x height, prefilled with char c
makeGrid :: Int -> Int -> Char -> Grid
makeGrid width height c = replicate height (replicate width c)

-- count of on pixel per grid
activePixels :: Grid -> Int
activePixels = sum . map (length . filter (== on))

example :: IO ()
example = do
  -- screen
  let screen = makeGrid 7 3 off
  -- commands
  let commands = [Rect 3 2, ColumnRotate 1 1, RowRotate 0 4, ColumnRotate 1 1]
  -- print result
  print $ activePixels (foldl execCommand screen commands)

problem1 :: IO ()
problem1 = do
  input <- readFile "input/input.txt"
  let commands = parseCommands input
  let screen = makeGrid 50 6 off
  putStrLn $ "Problem 1: " ++ show (activePixels (foldl execCommand screen commands))

problem2 :: IO ()
problem2 = do
  input <- readFile "input/input.txt"
  let commands = parseCommands input
  let screen = makeGrid 50 6 off
  -- solution: AFBUPZBJPS
  putStrLn $ "Problem2: \n" ++ show (foldl execCommand screen commands)

main :: IO ()
main = do
  problem1
  problem2
