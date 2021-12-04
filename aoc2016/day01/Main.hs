module Main where

import System.IO
import Data.Maybe
import Data.List
import Text.ParserCombinators.ReadP

-- a point on a grid
data Point = Point {x :: Integer, y :: Integer}
    deriving (Eq, Ord, Show)

-- the direction a person looks
data Direction = N | S | E | W
    deriving (Show)

-- a person, standing at location, looking in direction
data Person = Person {
    location :: Point,
    direction :: Direction,
    visited :: [Point] }
    deriving (Show)

-- rotate left or right
data Rotation = L | R
    deriving (Show)

-- walk steps unit in the given direction
type Steps = Integer

-- a complete move, consisting of a rotation and a number of steps
data Move = Move { rotation :: Rotation, steps :: Steps }
    deriving (Show)

-- parse a comma
commaP :: ReadP Char
commaP = char ','

-- parse a rotation, L|R
rotationP :: ReadP Rotation
rotationP = do
    rot <- choice [char c | c <- "LR"]
    case rot of
        'L' -> return L
        'R' -> return R

-- parse a digit
digitP :: ReadP Char
digitP = satisfy (\char -> char >= '0' && char <= '9')

-- parse a step, many digits
stepP :: ReadP Integer
stepP = do 
    parse <- many1 digitP
    return (read parse)

-- parse a single move
moveP :: ReadP Move
moveP = do
    skipMany commaP             -- comma, optional
    skipSpaces                  -- as the name says
    rotation <- rotationP       -- rotation, L | R
    steps <- stepP              -- steps, digits, at least one
    return (Move rotation steps)

-- parse a list of moves
movesP :: ReadP [Move]
movesP = many1 moveP

-- parse a list of moves and get the result,
-- the first of the pair of the last list entry
parseMoves :: String -> [Move]
parseMoves s = fst $ head $ reverse $ readP_to_S movesP s

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
stepsInXDirection i W = (-i)
stepsInXDirection _ _ = 0

-- convert steps into a direction into a y value
stepsInYDirection :: Steps -> Direction -> Steps
stepsInYDirection i N = i
stepsInYDirection i S = (-i)
stepsInYDirection _ _ = 0

-- walk a single move,
-- remember all locations between start and end location,
-- and return an updated person
walkSingle :: Move -> Person -> Person
walkSingle m p = new_person
    where
        new_direction = changeDirection (direction p) (rotation m)
        new_x = (x (location p))
            + (stepsInXDirection (steps m) new_direction)
        new_y = (y (location p))
            + (stepsInYDirection (steps m) new_direction)
        new_location = Point { x = new_x, y = new_y }
        new_visited = (visited p) ++ (Main.between (location p) new_location)
        new_person = Person {
            location = new_location,
            direction = new_direction,
            visited = new_visited }

-- walk a list of moves with a goven person,
-- return person at the end
-- same as walk, but with remembered places
walk :: [Move] -> Person -> Person
walk [] p = p
walk (m:ms) p = walk ms (walkSingle m p)

-- manhattan (or taxicab distance) between two points,
-- the absolute difference of their coordinates
manhattan :: Point -> Point -> Integer
manhattan p1 p2 = (abs ((x p1) - (x p2))) + (abs ((y p1) - (y p2)))

-- find duplicates in a given list,
-- if none or list is empty or has only one element, return unit
duplicates :: (Eq a) => [a] -> [a]
duplicates [] = []
duplicates (x:[]) = []
duplicates (x:y:xs) =
    reverse $ dup (x:y:xs) []
    where
        dup [] store = store
        dup (x:xs) store
            | elem x xs = dup xs (x:store)
            | otherwise = dup xs store

-- create a list of points between two given points, [p1 p2)
-- beware: points must differ only in the x OR the y coordinate
between :: Point -> Point -> [Point]
between p1 p2
    | (x p1) < (x p2) = [Point {x = _x, y = (y p1)} | _x <- [(x p1) .. (pred (x p2))]]
    | (y p1) < (y p2) = [Point {y = _y, x = (x p1)} | _y <- [(y p1) .. (pred (y p2))]]
    | (x p1) > (x p2) = [Point {x = _x, y = (y p1)} | _x <- [(x p1),(pred (x p1)) .. (succ (x p2))]]
    | (y p1) > (y p2) = [Point {y = _y, x = (x p1)} | _y <- [(y p1),(pred (y p1)) .. (succ (y p2))]]
    | otherwise = []

-- find the solution to problem 1,
-- walk the given moves and calculate the shortest
-- distance from the starting point to the target
-- distance is manhattan style, not euclidean
problem1 :: IO ()
problem1 = do
    let start = Point { x = 0, y = 0}
    let agent = Person { location = start, direction = N, visited = [] }
    putStrLn "Problem 1"
    content <- readFile "input/input.txt"
    let hq = location $ walk (parseMoves content) agent
    putStrLn $ (show $ manhattan start hq) ++ " Steps"

-- find the solution to problem 2,
-- same as problem 1, but hq is at the first location
-- we visited twice
-- and location means _any_ location,
-- not just between moves
problem2 :: IO ()
problem2 = do
    let start = Point { x = 0, y = 0}
    let agent = Person { location = start, direction = N, visited = [] }
    putStrLn "Problem 2"
    content <- readFile "input/input.txt"
    let hq = head $ duplicates $ visited $ walk (parseMoves content) agent
    putStrLn $ (show $ manhattan start hq) ++ " Steps"

-- solve all problems    
main :: IO ()
main = do
    problem1
    problem2
    