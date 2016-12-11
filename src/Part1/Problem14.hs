module Problem14 where

import           Data.Function (on)
import           Data.List     (maximumBy)
--
-- Problem 14: Longest Collatz sequence
--
-- The following iterative sequence is defined for the set of positive integers:
--
-- n → n/2 (n is even)
-- n → 3n + 1 (n is odd)
--
-- Using the rule above and starting with 13, we generate the following sequence:
--
-- 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
--
-- It can be seen that this sequence (starting at 13 and finishing at 1)
-- contains 10 terms. Although it has not been proved yet (Collatz Problem),
-- it is thought that all starting numbers finish at 1.
--
-- Which starting number, under one million, produces the longest chain?
--
-- NOTE: Once the chain starts the terms are allowed to go above one million.

problem14 :: Int
problem14 = maximumBy (compare `on` collatzLength) [1..999999]

isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

collatzLength :: Int -> Int
collatzLength 1 = 1
collatzLength x = 1 + collatzLength x' where
  x' | isEven x  = x `div` 2
     | otherwise = 3 * x + 1
