module Parser where

import Data.List
import Text.ParserCombinators.ReadP

type Triangle = (Int, Int, Int)

-- parse a digit
digitP :: ReadP Char
digitP = satisfy (\char -> char >= '0' && char <= '9')

-- parse 1, 2 or 3 digits
oneTwoThreeP :: ReadP String
oneTwoThreeP = (count 3 digitP) <++ (count 2 digitP) <++ (count 1 digitP)

-- parse an triangle
triangleP :: ReadP Triangle
triangleP = do
    skipSpaces
    first <- oneTwoThreeP
    skipSpaces
    second <- oneTwoThreeP
    skipSpaces
    third <- oneTwoThreeP
    return (read first, read second, read third)
    
-- parse a lines of input
linesP :: ReadP [Triangle]
linesP = many1 triangleP

-- remove the newlines and push everything back into a string
prepString :: String -> String
prepString s = unwords $ lines s

-- parse a list of triangles and return the result
parseTriangles :: String -> [Triangle]
parseTriangles s = fst $ head $ reverse $ readP_to_S linesP $ prepString s

-- like prepString, but also transpose the columns and rows
columnize :: String -> String
columnize s = unwords $ concat $ transpose $ map words $ lines s

-- parse a list of triangle, written in columns, and return result
parseTriangleColumns :: String -> [Triangle]
parseTriangleColumns s = fst $ head $ reverse $ readP_to_S linesP $ columnize s

-- content = "   123 456 789\n  321  654   987"
-- concat $ transpose $ map words $ lines "1 2 3\n4 5 6\n7 8 9"