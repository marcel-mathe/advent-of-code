module Main where

-- heavily inspired from:
-- https://wickstrom.tech/finite-state-machines/2017/11/10/finite-state-machines-part-1-modeling-with-haskell.html
-- "Finite-State Machines, Part 1: Modeling with Haskell Data Types", Oskar Wickström, 2017/11/10

import System.IO

import Types
import Fsm
import Parser

-- example input
example :: IO ()
example = do
    putStr "Example (enter)\n---\nThe Number is: "
    runFsm enter Q5 [U,L,L,P,R,R,D,D,D,P,L,U,R,D,L,P,U,U,U,U,D,P]
    putStrLn ""
    putStr "Example (executive)\n---\nThe Code is: "
    runFsm executive Q5 [U,L,L,P,R,R,D,D,D,P,L,U,R,D,L,P,U,U,U,U,D,P]
    putStrLn ""

problem1 :: IO ()
problem1 = do
    putStrLn "Problem 1\n---"
    content <- readFile "input/input.txt"
    runFsm enter Q5 (parseEvents content)
    putStrLn ""

problem2 :: IO ()
problem2 = do
    putStrLn "Problem 2\n---"
    content <- readFile "input/input.txt"
    runFsm executive Q5 (parseEvents content)
    putStrLn ""

main :: IO ()
main = do
    problem1
    problem2
