module Main where

import System.IO

import Parser

-- change direction according to a rotation command
changeDirection :: Direction -> Rotation -> Direction
changeDirection N L = W
changeDirection N R = E
changeDirection E L = N
changeDirection E R = S
changeDirection S L = E
changeDirection S R = W
changeDirection W L = S
changeDirection W R = N

-- convert steps into a direction into a x value
stepsInXDirection :: Steps -> Direction -> Steps
stepsInXDirection i E = i
stepsInXDirection i W = -i
stepsInXDirection _ _ = 0

-- convert steps into a direction into a y value
stepsInYDirection :: Steps -> Direction -> Steps
stepsInYDirection i N = i
stepsInYDirection i S = -i
stepsInYDirection _ _ = 0

-- walk a single move,
-- remember all locations between start and end location,
-- and return an updated person
walkSingle :: Person -> Move -> Person
walkSingle p m = Person new_location new_direction new_visited
    where
        new_direction = changeDirection (direction p) (rotation m)
        new_x = x (location p) + stepsInXDirection (steps m) new_direction
        new_y = y (location p) + stepsInYDirection (steps m) new_direction
        new_location = Point new_x new_y
        new_visited = visited p ++ Main.between (location p) new_location

-- walk a list of moves with a goven person,
-- return person at the end
-- same as walk, but with remembered places
walk :: Person -> [Move] -> Person
walk = foldl walkSingle

-- manhattan (or taxicab distance) between two points,
-- the absolute difference of their coordinates
manhattan :: Point -> Point -> Integer
manhattan p1 p2 = abs (x p1 - x p2) + abs (y p1 - y p2)

-- find duplicates in a given list,
-- if none or list is empty or has only one element, return unit
duplicates :: (Eq a) => [a] -> [a]
duplicates [] = []
duplicates [x] = []
duplicates (x:y:xs) =
    reverse $ dup (x:y:xs) []
    where
        dup [] store = store
        dup (x:xs) store
            | x `elem` xs = dup xs (x:store)
            | otherwise = dup xs store

-- create a list of points between two given points, [p1 p2)
-- beware: points must differ only in the x OR the y coordinate
between :: Point -> Point -> [Point]
between p1 p2
    | x p1 < x p2 = [Point {x = _x, y = y p1} | _x <- [(x p1) .. (pred (x p2))]]
    | y p1 < y p2 = [Point {y = _y, x = x p1} | _y <- [(y p1) .. (pred (y p2))]]
    | x p1 > x p2 = [Point {x = _x, y = y p1} | _x <- [(x p1),(pred (x p1)) .. (succ (x p2))]]
    | y p1 > y p2 = [Point {y = _y, x = x p1} | _y <- [(y p1),(pred (y p1)) .. (succ (y p2))]]
    | otherwise = []

-- find the solution to problem 1,
-- walk the given moves and calculate the shortest
-- distance from the starting point to the target
-- distance is manhattan style, not euclidean
problem1 :: IO ()
problem1 = do
    let start = Point 0 0
    let agent = Person start N []
    putStrLn "Problem 1"
    content <- readFile "input/input.txt"
    let hq = location $ walk agent (parseMoves content)
    putStrLn $ show (manhattan start hq) ++ " Steps"

-- find the solution to problem 2,
-- same as problem 1, but hq is at the first location
-- we visited twice
-- and location means _any_ location,
-- not just between moves
problem2 :: IO ()
problem2 = do
    let start = Point 0 0
    let agent = Person start N []
    putStrLn "Problem 2"
    content <- readFile "input/input.txt"
    let hq = head $ duplicates $ visited $ walk agent (parseMoves content)
    putStrLn $ show (manhattan start hq) ++ " Steps"

-- solve all problems    
main :: IO ()
main = do
    problem1
    problem2
    