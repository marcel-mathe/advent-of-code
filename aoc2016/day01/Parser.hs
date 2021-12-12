module Parser where

import Data.List
import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

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
digitP = satisfy isDigit

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
    Move rotation <$> stepP     -- steps, digits, at least one

-- parse a list of moves
movesP :: ReadP [Move]
movesP = many1 moveP

-- parse a list of moves and get the result,
-- the first of the pair of the last list entry
parseMoves :: String -> [Move]
parseMoves s = fst $ last (readP_to_S movesP s)
