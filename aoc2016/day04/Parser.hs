module Parser where

import Text.ParserCombinators.ReadP

-- checksum must be 5 most characters,
-- sorted by count (desc)
data Room = Room { name :: String, sector :: Int, check :: String}
    deriving (Show)

remDash :: String -> String
remDash = filter (\c -> c /= '-') 

lowerCaseCharP :: ReadP Char
lowerCaseCharP = satisfy (\char -> char >= 'a' && char <= 'z')

digitP :: ReadP Char
digitP = satisfy (\char -> char >= '0' && char <= '9')

nameP :: ReadP String
nameP = many1 lowerCaseCharP

codeP :: ReadP String
codeP = many1 digitP

checkP :: ReadP String
checkP = between (char '[') (char ']') (many1 lowerCaseCharP)

roomP :: ReadP Room
roomP = do
    n <- nameP
    c <- codeP
    ch <- checkP
    return Room { name=n, sector=(read c), check=ch}

parseRoom :: String -> Room
parseRoom s = fst $ head $ readP_to_S roomP $ remDash s

parseRooms :: String -> [Room]
parseRooms s = map parseRoom (lines s)
