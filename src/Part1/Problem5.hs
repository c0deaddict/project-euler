module Problem5 where

import Data.List (find)

--
-- Problem 5: Smallest multiple
--
-- 2520 is the smallest number that can be divided by each of the numbers from 1
-- to 10 without any remainder.
--
-- What is the smallest positive number that is evenly divisible by all of the
-- numbers from 1 to 20?

isDivisible :: Int -> Int -> Bool
isDivisible x y = x `mod` y  == 0

isDivisibleByAll :: [Int] -> Int -> Bool
isDivisibleByAll ys x = all (isDivisible x) ys

problem5 :: Maybe Int
problem5 = find (isDivisibleByAll [2..20]) [2520..]
