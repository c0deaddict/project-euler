module Problem20 where

import           Data.Char (digitToInt)

--
-- Problem 20: Factorial digit sum
--
-- n! means n × (n − 1) × ... × 3 × 2 × 1
--
-- For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
-- and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
--
-- Find the sum of the digits in the number 100!

problem20 :: Int
problem20 = sum $ digits $ fac 100

digits :: Integer -> [Int]
digits = map digitToInt . show

fac :: Integer -> Integer
fac 1 = 1
fac n = n * fac (n - 1)
