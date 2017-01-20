module Part2.Problem58 where

import           Data.List       (transpose)
import           Data.Ratio
import           Part1.Problem27 (isPrime)
import           Part1.Problem28 (diagonalInf)

--
-- Problem 58: Spiral primes
--
-- Starting with 1 and spiralling anticlockwise in the following way, a square
-- spiral with side length 7 is formed.
--
-- 37 36 35 34 33 32 31
-- 38 17 16 15 14 13 30
-- 39 18  5  4  3 12 29
-- 40 19  6  1  2 11 28
-- 41 20  7  8  9 10 27
-- 42 21 22 23 24 25 26
-- 43 44 45 46 47 48 49
--
-- It is interesting to note that the odd squares lie along the bottom right
-- diagonal, but what is more interesting is that 8 out of the 13 numbers lying
-- along both diagonals are prime; that is, a ratio of 8/13 ≈ 62%.
--
-- If one complete new layer is wrapped around the spiral above, a square spiral
-- with side length 9 will be formed. If this process is continued, what is the
-- side length of the square spiral for which the ratio of primes along both
-- diagonals first falls below 10%?

problem58 :: Int
problem58 = 1 + 2 * (1 + spiralLength) where
  spiralLength = length $ takeWhile (>= 1 % 10) primePercentage

spirals :: [[Int]]
spirals = transpose $ diagonalInf <$> [2,4,6,8]

spiralPrimes :: [Int]
spiralPrimes = length . filter isPrime <$> spirals

primePercentage :: [Ratio Int]
primePercentage = go spiralPrimes 0 1 where
  go (x:xs) total count =
    let
      total' = total + x
      count' = count + 4
      percentage = total' % count'
    in percentage : go xs total' count'

ratioToFloat :: Floating a => Ratio Int -> a
ratioToFloat r = fromIntegral (numerator r) / fromIntegral (denominator r)
