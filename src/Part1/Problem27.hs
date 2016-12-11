module Part1.Problem27 where

import           Data.Function (on)
import           Data.List     (group, maximumBy, sort)

--
-- Problem 27: Quadratic primes
--
-- Euler discovered the remarkable quadratic formula:
--
-- n^2+n+41
--
-- It turns out that the formula will produce 40 primes for the consecutive
-- integer values 0≤n≤39. However, when n=40,40^2+40+41=40(40+1)+41 is divisible
-- by 41, and certainly when n=41,41^2+41+41 is clearly divisible by 41.
--
-- The incredible formula n^2−79n+1601 was discovered, which produces
-- 80 primes for the consecutive values 0≤n≤79. The product of the
-- coefficients, −79 and 1601, is −126479.
--
-- Considering quadratics of the form:
--
-- n^2+an+b, where |a|<1000 and |b|≤1000
--
-- where |n| is the modulus/absolute value of n
-- e.g. |11|=11 and |−4|=4
--
-- Find the product of the coefficients, a and b, for the quadratic expression
-- that produces the maximum number of primes for consecutive values of n,
-- starting with n=0.

problem27 :: Int
problem27 = a * b where
  (a, b) = fst $ maximumBy (compare `on` snd) searchDomain

searchDomain :: [((Int, Int), Int)]
searchDomain =
  [ ((a, b), quadPrimesLength a b)
  | a <- [-999..999]
  , b <- [-1000..1000]
  ]

quadFn :: Int -> Int -> Int -> Int
quadFn a b n = n^2 + a*n + b

quadPrimes :: (Int -> Int) -> Int -> Int -> [Int]
quadPrimes f a b = takeWhile isPrime $ map (quadFn a b . f) [0..]

quadPrimesLength :: Int -> Int -> Int
quadPrimesLength a b = num pos where
  pos = quadPrimes id a b
  -- neg = quadPrimes negate a b
  -- num = length . group . sort
  num = length

-- https://stackoverflow.com/a/11769856/248948
isPrime :: Int -> Bool
isPrime n | n <= 1 = False
isPrime n = go 2 where
  go d
    | d*d > n        = True
    | n `mod` d == 0 = False
    | otherwise      = go (d+1)

primes = filter isPrime [2..]
