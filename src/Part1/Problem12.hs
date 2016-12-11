module Problem12 where

import           Data.List (find, group, permutations, subsequences)
import           Problem3  (altPrimeSieve, squareRoot, trialDivision)

--
-- Problem 12: Highly divisible triangular number
--
-- The sequence of triangle numbers is generated by adding the natural numbers.
-- So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The
-- first ten terms would be:
--
-- 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
--
-- Let us list the factors of the first seven triangle numbers:
--
--   1:  1
--   3:  1,3
--   6:  1,2,3,6
--   10: 1,2,5,10
--   15: 1,3,5,15
--   21: 1,3,7,21
--   28: 1,2,4,7,14,28
--
-- We can see that 28 is the first triangle number to have over five divisors.
--
-- What is the value of the first triangle number to have over five hundred
-- divisors?

problem12 :: Int -> Integer -> [Integer]
problem12 divisors primeLimit = numbers where
  coeffs = divisions divisors
  primes = altPrimeSieve primeLimit
  makeNumber _ []          = 1
  makeNumber [] _          = error "shouldn't happen"
  makeNumber (p:ps) (c:cs) = p^(c-1) * makeNumber ps cs
  makeNumbers cs = map (`makeNumber` cs) $ combinations (length cs) primes
  numbers = concatMap makeNumbers coeffs

-- This one actually found the answer, but the above approach is more fun.
altProblem12 :: Maybe Integer
altProblem12 = find (\x -> nrDivisors x > 500) triangleNumbers

triangleNumber :: Integer -> Integer
triangleNumber x = (x*x + x) `div` 2

triangleNumbers :: [Integer]
triangleNumbers = map triangleNumber [1..]

--
-- A triangle number is:
--   x = n(n + 1) / 2
--     = (n^2 + n) / 2
--     = 1/2n^2 + 1/2n
--   0 = 1/2n^2 + 1/2n - x
--
-- Solve this with the quadratic equation and you get:
--   2n = sqrt(1 + 8x) - 1
--
isTriangle :: Integer -> Bool
isTriangle x = sqr == y && restN == 0 where
  y = 1 + 8*x
  root = squareRoot y   -- approximate root, result is floored
  sqr = root * root     -- square to verify root is correct
  twoN = root - 1
  restN = twoN `mod` 2

-- Number of divisors = sum of all (prime factor + 1)
nrDivisors :: Integer -> Integer
nrDivisors x = product $ map (fromIntegral . (+) 1 . length) $ group $ trialDivision x

divisions :: Int -> [[Int]]
divisions x = map (terms primes) lengths  where
  primes = trialDivision x
  lengths = divide (fromIntegral $ length primes)
  terms _ []     = []
  terms p (x:xs) = product (take x p) : terms (drop x p) xs

-- Divide an integer in all possible additions
divide :: Int -> [[Int]]
divide = divide' [] where
  divide' p 0 = [p]
  divide' p n = concat [divide' (x : p) (n - x) | x <- [1..n]]

-- https://mail.haskell.org/pipermail/beginners/2011-November/008993.html
combinations n = filter (\s -> length s == n) . subsequences
