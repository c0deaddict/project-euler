module Part2.Problem53 where

--
-- Problem 53: Combinatoric selections
--
-- There are exactly ten ways of selecting three from five, 12345:
--
-- 123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
--
-- In combinatorics, we use the notation, 5 C 3 = 10.
--
-- In general,
--
-- n C r =	n! / r!(n−r)!
--
--   where r ≤ n, n! = n×(n−1)×...×3×2×1, and 0! = 1.
--
-- It is not until n = 23, that a value exceeds one-million: 23 C 10 = 1144066.
--
-- How many, not necessarily distinct, values of  n C r, for 1 ≤ n ≤ 100, are
-- greater than one-million?

problem53 :: Int
problem53 = length
  [ (n, r)
  | n <- [23..100]
  , r <- [1..n-1]
  , n `nCr` r > 10^6
  ]

fac :: Integer -> Integer
fac 1 = 1
fac n = n * fac (n - 1)

nCr :: Integer -> Integer -> Integer
nCr n r | r >= n = 0
nCr n r = fac n `div` (fac r * fac (n - r))
