module Part2.Problem48 where

--
-- Problem 48: Self powers
--
-- The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
--
-- Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

problem48 :: Integer
problem48 = sum [x^x | x <- [1..1000]] `mod` 10^10
