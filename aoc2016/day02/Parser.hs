module Parser where

import Text.ParserCombinators.ReadP

import Types

newlineP :: ReadP NumpadEvent
newlineP = do
    string "\n"
    return P

-- parse an event
eventP :: ReadP NumpadEvent
eventP = do
    ev <- choice [char c | c <- "UDRL"]
    case ev of
        'U' -> return U
        'D' -> return D
        'R' -> return R
        'L' -> return L

-- parse a single event
singleEventP :: ReadP NumpadEvent
singleEventP = do
    choice [eventP, newlineP]

-- parse a list of events
eventsP :: ReadP [NumpadEvent]
eventsP = many1 singleEventP

-- parse a list of events and get the result
parseEvents :: String -> [NumpadEvent]
parseEvents s = fst $ head $ reverse $ readP_to_S eventsP s
