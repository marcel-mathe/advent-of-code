-- mix from different solutions i saw online
module Main where

-- amount of times a sliding window over list xs
-- with wsize size was smaller than the next window
compute :: Ord b => Int -> [b] -> Int
compute wsize xs = length $ filter id $ zipWith (<) xs $ drop wsize xs

-- convert input string to list of numbers
prep :: String -> [Int]
prep s = map (\x -> read x :: Int) (lines s)

-- example problem, solutions: 7, 5
example :: IO ()
example = do
    let xs = "199\n200\n208\n210\n200\n207\n240\n269\n260\n263\n"
    putStrLn $ "Example (Part 1): " ++ (show $ compute 1 $ prep xs)
    putStrLn $ "Example (Part 2): " ++ (show $ compute 3 $ prep xs)

problem1 :: IO ()
problem1 = do
    xs <- readFile "input/input.txt"
    putStrLn $ "Part 1: " ++ (show $ compute 1 $ prep xs)

problem2 :: IO ()
problem2 = do
    xs <- readFile "input/input.txt"
    putStrLn $ "Part 2: " ++ (show $ compute 3 $ prep xs)

main :: IO ()
main = do
    problem1
    problem2
