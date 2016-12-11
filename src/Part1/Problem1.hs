module Part1.Problem1 where

multipleOf :: Int -> Int -> Bool
multipleOf x y = y `rem` x == 0

multiplesOf3And5 :: [Int] -> [Int]
multiplesOf3And5 = filter (\x -> multipleOf 3 x || multipleOf 5 x)

sumOfMultiplesOf3And5 :: Int -> Int
sumOfMultiplesOf3And5 limit = sum $ multiplesOf3And5 [1..(limit-1)]


-- sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]
