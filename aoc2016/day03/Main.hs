module Main where

import System.IO
import Data.List

import Parser

-- a valid triangle is a triangle where the length of
-- two sides is bigger than the third
validTriangle :: Triangle -> Bool
validTriangle t = all triangleSum (permutations $ triangleToList t)
    where
        triangleToList t = head $ [[x, y, z] | (x, y, z) <- [t]]
        triangleSum t = (t !! 0) + (t !! 1) > (t !! 2)

-- check a list of triangles
checkTriangles :: [Triangle] -> [Bool]
checkTriangles = map validTriangle

-- count True in list
countTrue :: [Bool] -> Int
countTrue bs = foldr (\b acc -> if b then acc + 1 else acc) 0 bs

problem1 :: IO ()
problem1 = do
    putStr "Problem 1: "
    content <- readFile "input/input.txt"
    putStrLn $ show $ countTrue $ checkTriangles $ parseTriangles content

problem2 :: IO ()
problem2 = do
    putStr "Problem 2: "
    content <- readFile "input/input.txt"
    putStrLn $ show $ countTrue $ checkTriangles $ parseTriangleColumns content

main :: IO ()
main = do
    problem1
    problem2
