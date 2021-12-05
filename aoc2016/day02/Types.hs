module Types where

-- a state per number on the numpad
data NumpadState = Q1 | Q2 | Q3 | Q4 | Q5 | Q6 | Q7 | Q8 | Q9 | QA | QB | QC | QD
    deriving (Show, Eq)

-- the state change events
-- Up, Down, Left, Right, Print, Null
data NumpadEvent = U | D | L | R | P | N
    deriving (Show, Eq)

-- finite state machine type alias
type FSM s e = s -> e -> IO s
