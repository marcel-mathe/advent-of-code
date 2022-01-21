module Parser where

import Data.Char (isDigit)
import Text.ParserCombinators.ReadP
import Types

-- remove the newlines and push everything back into a string
prepString :: String -> String
prepString s = unwords $ lines s

-- parse a digit
digitP :: ReadP Char
digitP = satisfy isDigit

rectP :: ReadP Command
rectP = do
  skipSpaces
  string "rect "
  a <- many1 digitP
  char 'x'
  b <- many1 digitP
  return $ Rect (read a :: Int) (read b :: Int)

rowP :: ReadP Command
rowP = do
  skipSpaces
  string "rotate row y="
  a <- many1 digitP
  string " by "
  b <- many1 digitP
  return $ RowRotate (read a :: Int) (read b :: Int)

columnP :: ReadP Command
columnP = do
  skipSpaces
  string "rotate column x="
  a <- many1 digitP
  string " by "
  b <- many1 digitP
  return $ ColumnRotate (read a :: Int) (read b :: Int)

singleCommandP :: ReadP Command
singleCommandP = choice [rectP, rowP, columnP]

commandsP :: ReadP [Command]
commandsP = many1 singleCommandP

parseCommands :: String -> [Command]
parseCommands s = fst $ last (readP_to_S commandsP $ prepString s)
