module Part2.Problem47 where

import           Data.List      (group, sort)
import           Part1.Problem3 (trialDivision)

--
-- Problem 47: Distinct primes factors
--
-- The first two consecutive numbers to have two distinct prime factors are:
--
-- 14 = 2 × 7
-- 15 = 3 × 5
--
-- The first three consecutive numbers to have three distinct prime factors are:
--
-- 644 = 2² × 7 × 23
-- 645 = 3 × 5 × 43
-- 646 = 2 × 17 × 19.
--
-- Find the first four consecutive integers to have four distinct prime factors
-- each. What is the first of these numbers?

problem47 :: Int
problem47 = head
  [ x
  | x <- [1..]
  , let ys = [x, x+1, x+2, x+3]
  , distinctPrimeFactors ys 4
  ]

primeFactors :: Int -> [Int]
primeFactors x = product <$> group (trialDivision x)

distinctPrimeFactors :: [Int] -> Int -> Bool
distinctPrimeFactors xs len = correctLength && uniqueFactors == numFactors where
  factors = map primeFactors xs
  correctLength = all ((==) len . length) factors
  numFactors = sum $ map length factors
  uniqueFactors = length $ group $ sort $ concat factors
