module Types where

-- a state per button on the numpad
data NumpadState = Q1 | Q2 | Q3 | Q4 | Q5 | Q6 | Q7 | Q8 | Q9 | QA | QB | QC | QD
    deriving (Show, Eq)

-- the state change events
-- Up, Down, Left, Right, Print
data NumpadEvent = U | D | L | R | P
    deriving (Show, Eq)

-- finite state machine type alias
type FSM s e = s -> e -> IO s
