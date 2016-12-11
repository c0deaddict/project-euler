module Part2.Problem46 where

import           Data.List       (find)
import           Part1.Problem27 (isPrime, primes)
import           Part1.Problem3  (squareRoot)

--
-- Problem 46: Goldbach's other conjecture
--
-- It was proposed by Christian Goldbach that every odd composite number can be
-- written as the sum of a prime and twice a square.
--
-- 9 = 7 + 2×1^2
-- 15 = 7 + 2×2^2
-- 21 = 3 + 2×3^2
-- 25 = 7 + 2×3^2
-- 27 = 19 + 2×2^2
-- 33 = 31 + 2×1^2
--
-- It turns out that the conjecture was false.
--
-- What is the smallest odd composite that cannot be written as the sum of a
-- prime and twice a square?

problem46 :: Maybe Int
problem46 = find (not . isGoldbach) oddComposites

oddComposites :: [Int]
oddComposites = filter (not . isPrime) [2*x + 1 | x <- [1..]]

isGoldbach :: Int -> Bool
isGoldbach n = any remainderIsSquare smallerPrimes where
  smallerPrimes = takeWhile (< n) primes
  remainderIsSquare p = 2*y*y == x where
    x = n - p
    y = squareRoot (x `div` 2)
