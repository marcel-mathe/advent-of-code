module Fsm where

import Control.Monad        (foldM)
import Text.Printf          (printf)

import Types

-- enter a number into the numpad
enter :: FSM NumpadState NumpadEvent
-- Q1
enter Q1 U = return Q1
enter Q1 R = return Q2
enter Q1 D = return Q4
enter Q1 L = return Q1
enter Q1 P = do
    putStr "1"
    return Q1
-- Q2
enter Q2 U = return Q2
enter Q2 R = return Q3
enter Q2 D = return Q5
enter Q2 L = return Q1
enter Q2 P = do
    putStr "2"
    return Q2
-- Q3
enter Q3 U = return Q3
enter Q3 R = return Q3
enter Q3 D = return Q6
enter Q3 L = return Q2
enter Q3 P = do
    putStr "3"
    return Q3
-- Q4
enter Q4 U = return Q1
enter Q4 R = return Q5
enter Q4 D = return Q7
enter Q4 L = return Q4
enter Q4 P = do
    putStr "4"
    return Q4
-- Q5
enter Q5 U = return Q2
enter Q5 R = return Q6
enter Q5 D = return Q8
enter Q5 L = return Q4
enter Q5 P = do
    putStr "5"
    return Q5
-- Q6
enter Q6 U = return Q3
enter Q6 R = return Q6
enter Q6 D = return Q9
enter Q6 L = return Q5
enter Q6 P = do
    putStr "6"
    return Q6
-- Q7
enter Q7 U = return Q4
enter Q7 R = return Q8
enter Q7 D = return Q7
enter Q7 L = return Q7
enter Q7 P = do
    putStr "7"
    return Q7
-- Q8
enter Q8 U = return Q5
enter Q8 R = return Q9
enter Q8 D = return Q8
enter Q8 L = return Q7
enter Q8 P = do
    putStr "8"
    return Q8
-- Q9
enter Q9 U = return Q6
enter Q9 R = return Q9
enter Q9 D = return Q9
enter Q9 L = return Q8
enter Q9 P = do
    putStr "9"
    return Q9

-- enter a number into the executive numpad
executive :: FSM NumpadState NumpadEvent
-- Q1
executive Q1 U = return Q1
executive Q1 R = return Q1
executive Q1 D = return Q3
executive Q1 L = return Q1
executive Q1 P = do
    putStr "1"
    return Q1
-- Q2
executive Q2 U = return Q2
executive Q2 R = return Q3
executive Q2 D = return Q6
executive Q2 L = return Q2
executive Q2 P = do
    putStr "2"
    return Q2
-- Q3
executive Q3 U = return Q1
executive Q3 R = return Q4
executive Q3 D = return Q7
executive Q3 L = return Q2
executive Q3 P = do
    putStr "3"
    return Q3
-- Q4
executive Q4 U = return Q4
executive Q4 R = return Q4
executive Q4 D = return Q8
executive Q4 L = return Q3
executive Q4 P = do
    putStr "4"
    return Q4
-- Q5
executive Q5 U = return Q5
executive Q5 R = return Q6
executive Q5 D = return Q5
executive Q5 L = return Q5
executive Q5 P = do
    putStr "5"
    return Q5
-- Q6
executive Q6 U = return Q2
executive Q6 R = return Q7
executive Q6 D = return QA
executive Q6 L = return Q5
executive Q6 P = do
    putStr "6"
    return Q6
-- Q7
executive Q7 U = return Q3
executive Q7 R = return Q8
executive Q7 D = return QB
executive Q7 L = return Q6
executive Q7 P = do
    putStr "7"
    return Q7
-- Q8
executive Q8 U = return Q4
executive Q8 R = return Q9
executive Q8 D = return QC
executive Q8 L = return Q7
executive Q8 P = do
    putStr "8"
    return Q8
-- Q9
executive Q9 U = return Q9
executive Q9 R = return Q9
executive Q9 D = return Q9
executive Q9 L = return Q8
executive Q9 P = do
    putStr "9"
    return Q9
-- QA
executive QA U = return Q6
executive QA R = return QB
executive QA D = return QA
executive QA L = return QA
executive QA P = do
    putStr "A"
    return QA
-- QB
executive QB U = return Q7
executive QB R = return QC
executive QB D = return QD
executive QB L = return QA
executive QB P = do
    putStr "B"
    return QB
-- QC
executive QC U = return Q8
executive QC R = return QC
executive QC D = return QC
executive QC L = return QB
executive QC P = do
    putStr "C"
    return QC
-- QD
executive QD U = return QB
executive QD R = return QD
executive QD D = return QD
executive QD L = return QD
executive QD P = do
    putStr "D"
    return QD

-- run state machine
runFsm :: Foldable f => FSM s e -> s -> f e -> IO s
runFsm = foldM

-- extra logging state
withLogging :: (Show s, Show e) => FSM s e -> FSM s e
withLogging fsm s e = do
  s' <- fsm s e
  printf "- %s × %s -> %s\n" (show s) (show e) (show s')
  return s'