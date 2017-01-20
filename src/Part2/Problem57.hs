module Part2.Problem57 where

import           Data.Ratio

--
-- Problem 57: Square root convergents
--
-- It is possible to show that the square root of two can be expressed as an
-- infinite continued fraction.
--
-- √ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...
--
-- By expanding this for the first four iterations, we get:
--
-- 1 + 1/2 = 3/2 = 1.5
-- 1 + 1/(2 + 1/2) = 7/5 = 1.4
-- 1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
-- 1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...
--
-- The next three expansions are 99/70, 239/169, and 577/408, but the eighth
-- expansion, 1393/985, is the first example where the number of digits in the
-- numerator exceeds the number of digits in the denominator.
--
-- In the first one-thousand expansions, how many fractions contain a numerator
-- with more digits than denominator?

problem57 :: (Show a, Integral a) => Int
problem57 = length $ filter candidate $ map sqrt2 [1..1000] where
  candidate x = numDigits (numerator x) > numDigits (denominator x)
  numDigits = length . show

sqrt2 :: Integral a => Int -> Ratio a
sqrt2 d = 1 + go d where
  go 0 = 0
  go d = 1 / (2 + go (d-1))
